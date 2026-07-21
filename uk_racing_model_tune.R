###############################################################################
# Horse Racing Classification - ML Finish Group Prediction
###############################################################################

suppressPackageStartupMessages(library(tidymodels))
library(bonsai)
library(yardstick)
library(probably)
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

tree_model = 'xgboost' 

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

n_classes  <- length(levels(train_data[[target_var]]))
best_metric <- if(n_classes > 2) 'accuracy' else 'roc_auc'
tune_metrics <- if(n_classes > 2) metric_set(accuracy) else metric_set(accuracy, roc_auc)

print(paste("Classes:", n_classes))

###############################################################################
# MODEL SPECIFICATION
###############################################################################

#### finish group target var
# Recipe for preprocessing, parameterised by over_ratio so each mode can supply
# either tune() (tuning mode) or a fixed numeric value (fixed mode). fit_resamples()
# requires ALL tune() placeholders resolved up front (unlike tune_grid()), so the
# recipe can't carry an unresolved tune() into the fixed-mode branch.
build_recipe <- function(over_ratio_val) {
  recipe(model_formula, data = train_data) %>%
    # Create dummy variables
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    # Remove zero variance predictors
    step_zv(all_predictors()) %>%
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors()) %>%
    # Remove highly correlated predictors
    step_corr(all_numeric_predictors(), threshold = 0.9) %>%
    # SMOTE on Win and Place classes
    step_smote(all_outcomes(), over_ratio = over_ratio_val, skip = TRUE)
  # interaction terms
  #step_interact(terms = ~ bsp_prob:kerdos_rating) 
}

# predictors summary uses the tuning-mode (tune()) version just for inspection
recipe_ml <- build_recipe(tune())

# class imbalance weight for xgboost, computed once from train_data
class_counts <- table(train_data[[target_var]])
pos_weight <- as.numeric(class_counts["Not_Win"] / class_counts["Win"])
print(paste("scale_pos_weight:", round(pos_weight, 2)))

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
  
  # recipe keeps over_ratio = tune(); tune_grid() resolves it per grid row
  workflow_ml <- workflow() %>%
    add_recipe(
      build_recipe(tune()),
      blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
    )
  
  tune_spec <- boost_tree(
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune()
  ) %>%
    set_mode('classification') %>%
    set_engine(tree_model, verbose = 1, scale_pos_weight = pos_weight)
  
  workflow_ml <- workflow_ml %>% add_model(tune_spec)
  
  tree_grid <- expand.grid(
    trees          = trees,
    min_n          = min_n,
    tree_depth     = tree_depth,
    learn_rate     = learn_rate,
    loss_reduction = loss_reduction,
    over_ratio     = c(0.5, 0.75, 1.0)   # SMOTE ratio now part of the grid
  ) %>% unique()
  
  print(paste('Grid size:', nrow(tree_grid), 'combinations'))
  
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
  
  over_ratio_fixed <- 0.75  # SMOTE minority over-sampling ratio; tune this by hand here
  print(paste('over_ratio:', over_ratio_fixed))
  
  # recipe built with over_ratio already resolved -- no tune() left anywhere,
  # which fit_resamples() requires (unlike tune_grid())
  workflow_ml <- workflow() %>%
    add_recipe(
      build_recipe(over_ratio_fixed),
      blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
    )
  
  tune_spec <- boost_tree(
    trees          = trees,
    min_n          = min_n,
    tree_depth     = tree_depth,
    learn_rate     = learn_rate,
    loss_reduction = loss_reduction
  ) %>%
    set_mode('classification') %>%
    set_engine(tree_model, verbose = 1, scale_pos_weight = pos_weight)
  
  workflow_ml <- workflow_ml %>% add_model(tune_spec)
  
  tune_results <- fit_resamples(
    workflow_ml,
    resamples = folds_ml,
    metrics   = tune_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE, allow_par = TRUE)
  )
  
  # final_params only needs the model-spec params now, since the recipe has no
  # remaining tune() placeholders (over_ratio was baked in via build_recipe() above)
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

print('=== PERFORMANCE METRICS (default 0.5 threshold) ===')
print(predictions %>% metrics(truth = !!sym(target_var), estimate = .pred_class))

print('=== CONFUSION MATRIX (default 0.5 threshold) ===')
print(predictions %>% conf_mat(truth = !!sym(target_var), estimate = .pred_class))

print('=== PER-CLASS ACCURACY (default 0.5 threshold) ===')
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
  event_col <- paste0(".pred_", levels(predictions[[target_var]])[1])
  roc_val <- predictions %>% roc_auc(truth = !!sym(target_var), !!sym(event_col))
  print(paste("AUC:", round(roc_val$.estimate, 4)))
} else {
  if(length(existing_cols) == n_classes) {
    print(predictions %>% roc_auc(truth = !!sym(target_var), all_of(existing_cols)))
  } else {
    print("WARNING: Mismatch in probability columns, skipping ROC")
  }
}

###############################################################################
# THRESHOLD TUNING (binary only)
###############################################################################

if(n_classes == 2) {
  
  print('=== THRESHOLD TUNING (classification metrics, for reference only) ===')
  
  # NOTE: j_index (and most classification metrics) treat false positives and false
  # negatives as equally costly. For betting, a false positive costs a real stake
  # while a false negative only costs a missed opportunity -- so we report j_index
  # here for reference, but select the OPERATING THRESHOLD below using actual ROI
  # via pnl_df()/pnl_table(), which is the metric that matches the real decision.
  threshold_results <- predictions %>%
    threshold_perf(truth = !!sym(target_var), estimate = .pred_Win,
                   thresholds = seq(0.05, 0.5, 0.02))
  
  best_j <- threshold_results %>%
    filter(.metric == "j_index") %>%
    arrange(desc(.estimate)) %>%
    slice(1)
  
  print(paste("Best threshold by Youden's J (reference only, not used to pick a cutoff):",
              best_j$.threshold))
  print(best_j)
  
  print('=== THRESHOLD TUNING (ROI-based, using pnl_df/pnl_table) ===')
  
  # sweep a probability threshold on winprob_adj (race-normalised win probability)
  # rather than the raw 0.5-style classification cutoff, since betting decisions
  # are naturally about "is this horse's probability high enough to back", which
  # pnl_df() already expresses via winprob_adj/conf_level/exp_value
  pnl_dat <- pnl_df(ranks = 1)  # top-ranked horse per race only, for this sweep
  
  prob_thresholds <- seq(0.05, 0.40, 0.05)
  
  roi_sweep <- purrr::map_dfr(prob_thresholds, function(p) {
    pnl_table(pnl_dat, vars = "bsp_index_all", val = -Inf, conf = -Inf, prob = p) %>%
      mutate(prob_threshold = p)
  })
  
  print(roi_sweep %>% select(prob_threshold, bets, sr, ave_win_price, profit, roi))
  
  # pick the threshold with the best ROI, subject to a minimum sample size so we're
  # not just chasing noise from a handful of bets -- adjust min_bets to taste
  min_bets <- 20
  best_roi_threshold <- roi_sweep %>%
    filter(bets >= min_bets) %>%
    arrange(desc(roi)) %>%
    slice(1)
  
  if(nrow(best_roi_threshold) > 0) {
    print(paste0("Best ROI threshold (prob > ", best_roi_threshold$prob_threshold,
                 "): ROI = ", round(best_roi_threshold$roi, 3),
                 " over ", best_roi_threshold$bets, " bets, strike rate ",
                 round(best_roi_threshold$sr, 3)))
  } else {
    print(paste("No threshold met the minimum bet count of", min_bets, "-- consider lowering min_bets or reviewing model calibration"))
  }
  
  # recompute predicted class at the classification-equivalent Youden's J threshold,
  # kept for comparison against the ROI-driven pick above
  predictions <- predictions %>%
    mutate(.pred_class_tuned = factor(
      ifelse(.pred_Win >= best_j$.threshold, "Win", "Not_Win"),
      levels = levels(.data[[target_var]])
    ))
  
  print('=== CONFUSION MATRIX (Youden J threshold, reference only) ===')
  print(predictions %>% conf_mat(truth = !!sym(target_var), estimate = .pred_class_tuned))
}

###############################################################################
# PER-RACE RANKING EVALUATION
###############################################################################

print('=== PER-RACE RANKING ACCURACY ===')

race_eval <- test_data %>%
  select(raceid, all_of(target_var)) %>%
  bind_cols(winprob = predictions$.pred_Win) %>%
  group_by(raceid) %>%
  mutate(
    winprob_rank  = dense_rank(-winprob),
    actual_winner = .data[[target_var]] == "Win"
  ) %>%
  ungroup()

top1_summary <- race_eval %>%
  group_by(raceid) %>%
  summarise(
    correct_top1 = any(winprob_rank == 1 & actual_winner),
    has_winner   = any(actual_winner)
  ) %>%
  filter(has_winner) %>%  # only races where the actual winner is present in test_data
  summarise(
    n_races       = n(),
    top1_accuracy = mean(correct_top1)
  )

print(top1_summary)

print('=== MARKET FAVOURITE BASELINE (top1 accuracy) ===')
# how often is the market's own favourite (shortest betfairwinsp) actually the winner?
# this is the number the model's top1_accuracy above needs to beat to be adding value
fav_baseline <- test_data %>%
  group_by(raceid) %>%
  mutate(fav_rank = dense_rank(betfairwinsp)) %>%   # rank 1 = shortest price = favourite
  ungroup() %>%
  group_by(raceid) %>%
  summarise(fav_wins = any(fav_rank == 1 & .data[[target_var]] == "Win")) %>%
  summarise(
    n_races           = n(),
    fav_top1_accuracy = mean(fav_wins)
  )

print(fav_baseline)
print(paste0(
  "Model top1_accuracy (", round(top1_summary$top1_accuracy, 3),
  ") vs favourite baseline (", round(fav_baseline$fav_top1_accuracy, 3), ")"
))

###############################################################################
# DIAGNOSTIC: bsp_prob-ONLY BASELINE MODEL
###############################################################################
# bsp_prob is derived directly from the market price and is very likely the
# dominant feature in the full model. This fits a minimal one-feature model on
# bsp_prob alone (same hyperparameters/imbalance handling, no SMOTE since a
# single continuous feature doesn't need it) to check whether the other six
# features (kerdos_rating_rank, draw_relative, distance_m, weightcarried,
# horseage, daysoff) are adding real signal or just noise. If this baseline's
# top1_accuracy matches or beats the full model's, the other features aren't
# currently earning their place.

print('=== DIAGNOSTIC: bsp_prob-ONLY BASELINE ===')

formula_bsp <- as.formula(paste(target_var, "~ bsp_prob"))

recipe_bsp <- recipe(formula_bsp, data = train_data) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# reuse the same fixed hyperparameters and class-imbalance weight as the main
# model for a fair comparison; deliberately no SMOTE here (single feature)
spec_bsp <- boost_tree(
  trees          = trees,
  min_n          = min_n,
  tree_depth     = tree_depth,
  learn_rate     = learn_rate,
  loss_reduction = loss_reduction
) %>%
  set_mode('classification') %>%
  set_engine(tree_model, verbose = 0, scale_pos_weight = pos_weight)

workflow_bsp <- workflow() %>%
  add_recipe(recipe_bsp) %>%
  add_model(spec_bsp)

final_model_bsp <- workflow_bsp %>% fit(data = train_data)

predictions_bsp <- final_model_bsp %>%
  predict(test_data, type = "prob") %>%
  bind_cols(test_data %>% select(all_of(target_var)))

race_eval_bsp <- test_data %>%
  select(raceid, all_of(target_var)) %>%
  bind_cols(winprob = predictions_bsp$.pred_Win) %>%
  group_by(raceid) %>%
  mutate(
    winprob_rank  = dense_rank(-winprob),
    actual_winner = .data[[target_var]] == "Win"
  ) %>%
  ungroup()

top1_summary_bsp <- race_eval_bsp %>%
  group_by(raceid) %>%
  summarise(
    correct_top1 = any(winprob_rank == 1 & actual_winner),
    has_winner   = any(actual_winner)
  ) %>%
  filter(has_winner) %>%
  summarise(
    n_races       = n(),
    top1_accuracy = mean(correct_top1)
  )

auc_bsp <- predictions_bsp %>% roc_auc(truth = !!sym(target_var), .pred_Win)

print(top1_summary_bsp)
print(paste("bsp_prob-only AUC:", round(auc_bsp$.estimate, 4)))

print('=== COMPARISON SUMMARY ===')
comparison <- tibble(
  approach      = c("Full model (7 features)", "bsp_prob only", "Market favourite"),
  n_races       = c(top1_summary$n_races, top1_summary_bsp$n_races, fav_baseline$n_races),
  top1_accuracy = c(top1_summary$top1_accuracy, top1_summary_bsp$top1_accuracy, fav_baseline$fav_top1_accuracy)
)
print(comparison)

print('=== COMPLETE ===')