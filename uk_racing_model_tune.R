###############################################################################
# Horse Racing Classification - ML Finish Group Prediction
###############################################################################

suppressPackageStartupMessages(library(tidymodels))
library(bonsai)
library(yardstick)
library(vip)

ch=function(x){as.character(x)}
fct=function(x){as.factor(x)}
nm=function(x){as.numeric(x)}
hms_times=function(x){format(as.POSIXct(x, origin="1970-01-01",tz="UTC"), '%H:%M')}

# gerenerate test pnl df 
pnl_df <- function(ranks){
  dat <- bind_cols(test_data, 
                   winprob=predictions$.pred_Win,
                   winclass=predictions$.pred_class,
                   winprob_sim=predictions_sim$.pred_Win,
                   betfairwinsp_sim=test_data_sim$betfairwinsp_sim) %>%
    group_by(raceid) %>%
    mutate(
      winprob_adj=winprob/sum(winprob),
      winprob_sim_adj=winprob_sim/sum(winprob_sim),
      winprob_rank=dense_rank(-winprob_adj),
      win=ifelse(positionofficial==1, 1, 0),
      win=ifelse(sum(win)==2 & win==1, 0.5, win) # dead-heat adj
    ) %>% ungroup() %>%
    mutate(bsp_bin=gsub('0-2.5', '1.01-2.5', bsp_bin),
           bsp_index_all=1,
           pick=ifelse(winprob_rank %in% ranks, 1, 0),
           winprice=1/winprob_adj, # model price
           winprice_sim=1/winprob_sim_adj, # sim model price
           ave_win_bsp=ifelse(win==1, betfairwinsp, NA),
           profit=win*betfairwinsp-1,
           bf_prob = 1/(betfairwinsp),
           exp_value = (betfairwinsp - winprice)/winprice,
           conf_level = winprob - bf_prob
    )
  return(dat)
}

# pnl summary table
pnl_table <- function(dat, vars, val, conf, prob){
  x <- dat %>%
    filter(pick==1,
           exp_value > val, 
           conf_level > conf,
           winprob_adj > prob,
    ) %>%
    group_by(across(all_of(vars))) %>% # bsp_index, bsp_bin
    summarise(bets=sum(pick), 
              sr=mean(win),  
              ave_win_price=mean(na.omit(ave_win_bsp)),
              profit=sum(profit), 
              roi=sum(profit)/sum(pick)) 
  return(x)
}
chart.rpart<-function(model, title){rpart.plot(model, digits=3, type=3, extra = 101, main=title)}

price_sim <- function(price_col){
  test_data %>%
    rename(price = !!sym(price_col)) %>%
    group_by(raceid) %>%
    mutate(
      total_prob = sum(1 / price),                # original overround (race total)
      is_fav = price == min(price),
      fav_price = nm(price[is_fav][1]),
      # jitter favourite by +/- 5 cents (one draw per race, applied only to the fav row)
      new_fav_price = fav_price + runif(1, -0.1, 0.1),
      new_fav_prob = 1 / new_fav_price,
      # implied prob of all non-favourites, original
      nonfav_prob_orig = sum((1 / price)[!is_fav]),
      # probability "budget" left for non-favourites after fixing the fav
      remaining_prob = total_prob - new_fav_prob,
      # scale each non-favourite's implied prob proportionally so they sum to remaining_prob
      scale_factor = remaining_prob / nonfav_prob_orig,
      new_prob = if_else(is_fav, new_fav_prob, (1 / price) * scale_factor),
      betfairwinsp_sim = 1 / new_prob
    ) %>%
    ungroup() %>%
    select(betfairwinsp_sim)
}


###############################################################################
# INITIALIZATION
###############################################################################

tree_model = TreeModel[t]  

print(paste0('Model:', tree_model, ' | Tuning: ', ifelse(tuneModels == 1, 'YES', 'NO')) )

folds_ml <- sliding_period(
  train_data,
  index = meetingdate,
  period = "month",
  lookback = Inf,        # expanding window
  assess_stop = 1
)

#folds_ml <- vfold_cv(train_data, v = 2, strata = finish_group)
#folds_ml <- validation_split(train_data, prop = 0.8, strata = finish_group)

n_classes  <- length(levels(train_data$finish_group))
best_metric <- if(n_classes > 2) 'accuracy' else 'roc_auc'
tune_metrics <- if(n_classes > 2) metric_set(accuracy) else metric_set(accuracy, roc_auc)

print(paste("Classes:", n_classes))

###############################################################################
# MODEL SPECIFICATION
###############################################################################

#### finish group target var
# Recipe for preprocessing
recipe_ml <- recipe(model_formula, data = train_data) %>%
  # Create dummy variables
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # Remove zero variance predictors
  step_zv(all_predictors()) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  # Remove highly correlated predictors
  step_corr(all_numeric_predictors(), threshold = 0.9) #%>%
  # SMOTE on Win and Place classes
  #step_smote(finish_group, over_ratio = 0.5, skip = TRUE) # %>%
  # interaction terms
  #step_interact(terms = ~ bsp_prob:kerdos_rating) 

# predictors
predictors = summary(recipe_ml)

# test sample
test_data <- model_data %>% 
  filter(meetingdate >= test_date_start,
         meetingdate < test_date_end
  )

# sim test sample
test_data_sim <- test_data %>% 
  bind_cols(price_sim('betfairwinsp')) %>%
  # recalculate bsp-linked features
  group_by(raceid) %>%
  mutate(bsp_prob = 1/betfairwinsp_sim,
         bsp_rank = rank(betfairwinsp_sim, ties.method="min")/numberofrunners,
         bsp_prob_run1_fluc = (bsp_prob - run1_bsp_prob)/run1_bsp_prob,
         bsp_prob_run1_fluc = ifelse(is.na(bsp_prob_run1_fluc), 0, bsp_prob_run1_fluc)) %>%
  ungroup()




print(paste0('test_perc_split = ', round(nrow(test_data)/nrow(train_data)*100, 1),'%'))


if(tuneModels == 1) {
  
  print('=== TUNING MODE ===')
  
  tune_spec <- boost_tree(
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune()
  ) %>%
    set_mode('classification') %>%
    set_engine(tree_model, verbose = 1)
  
  tree_grid <- expand.grid(
    trees          = trees,
    min_n          = min_n,
    tree_depth     = tree_depth,
    learn_rate     = learn_rate,
    loss_reduction = loss_reduction
  ) %>% unique()
  
  print(paste('Grid size:', nrow(tree_grid), 'combinations'))
  
  # Build workflow before tuning so recipe is applied inside each CV fold
  workflow_ml <- workflow() %>%
    add_recipe(
      recipe_ml,
      blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
    ) %>%
    add_model(tune_spec)
  
  tune_results <- tune_grid(
    workflow_ml,
    resamples = folds_ml,
    grid      = tree_grid,
    metrics   = tune_metrics,
    control   = control_grid(verbose = TRUE, save_pred = TRUE, allow_par = TRUE)
  )
  
  final_params <- tune_results %>% select_best(metric = best_metric)
  print('Best parameters:')
  print(final_params)
  
} else {
  
  print('=== FIXED PARAMETERS MODE ===')
  print(sprintf('trees=%d, min_n=%d, tree_depth=%d, learn_rate=%.3f, loss_reduction=%.3f',
                trees, min_n, tree_depth, learn_rate, loss_reduction))
  
  tune_spec <- boost_tree(
    trees          = trees,
    min_n          = min_n,
    tree_depth     = tree_depth,
    learn_rate     = learn_rate,
    loss_reduction = loss_reduction
  ) %>%
    set_mode('classification') %>%
    set_engine(tree_model, verbose = 1)
  
  # Build workflow before resampling so recipe is applied inside each CV fold
  workflow_ml <- workflow() %>%
    add_recipe(
      recipe_ml,
      blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
    ) %>%
    add_model(tune_spec)
  
  tune_results <- fit_resamples(
    workflow_ml,
    resamples = folds_ml,
    metrics   = tune_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE, allow_par = TRUE)
  )
  
  final_params <- tibble(
    trees          = trees,
    min_n          = min_n,
    tree_depth     = tree_depth,
    learn_rate     = learn_rate,
    loss_reduction = loss_reduction
  )
}

if(nrow(collect_metrics(tune_results)) == 0) stop("ERROR: Model fitting failed")

print('CV results:')
print(collect_metrics(tune_results))

###############################################################################
# FIT FINAL MODEL
###############################################################################

workflow_ml <- workflow_ml %>% finalize_workflow(final_params)

print('Fitting final model...')
final_model <- workflow_ml %>% fit(data = train_data)

print('Features used:')
print(names(final_model$pre$mold$predictors))

print('Feature importance:')
print(vip::vip(final_model, num_features = 20))

###############################################################################
# EVALUATION
###############################################################################

print('Generating predictions...')

predictions <- final_model %>%
  predict(test_data) %>%
  bind_cols(final_model %>% predict(test_data, type = "prob")) %>%
  bind_cols(test_data %>% select(all_of(target_var))) %>%
  rowwise() %>%
  mutate(max_pred_prob = max(c_across(starts_with(".pred_") & where(is.numeric)))) %>%
  ungroup()

# bsp sim predictions
predictions_sim <- final_model %>%
  predict(test_data_sim) %>%
  bind_cols(final_model %>% predict(test_data_sim, type = "prob"))

print('=== PERFORMANCE METRICS ===')
print(predictions %>% metrics(truth = !!sym(target_var), estimate = .pred_class))

print('=== CONFUSION MATRIX ===')
print(predictions %>% conf_mat(truth = !!sym(target_var), estimate = .pred_class))

print('=== PER-CLASS ACCURACY ===')
predictions %>%
  group_by(.data[[target_var]]) %>%
  summarise(
    n         = n(),
    accuracy  = mean(.pred_class == .data[[target_var]]),
    mean_prob = mean(max_pred_prob)
  ) %>%
  arrange(desc(accuracy)) %>%
  print()

print('=== ROC AUC ===')
prob_cols    <- paste0(".pred_", levels(predictions[[target_var]]))
existing_cols <- prob_cols[prob_cols %in% names(predictions)]

if(n_classes == 2) {
  roc_val <- predictions %>% roc_auc(truth = !!sym(target_var), all_of(existing_cols))
  print(paste("AUC:", round(roc_val$.estimate, 4)))
} else {
  if(length(existing_cols) == n_classes) {
    print(predictions %>% roc_auc(truth = !!sym(target_var), all_of(existing_cols)))
  } else {
    print("WARNING: Mismatch in probability columns, skipping ROC")
  }
}

print('=== COMPLETE ===')