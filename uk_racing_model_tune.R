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
                     ) %>%
      group_by(raceid) %>%
      mutate(
        winprob_adj=winprob/sum(winprob),
        winprob_rank=dense_rank(-winprob_adj),
        win=ifelse(positionofficial==1, 1, 0),
        win=ifelse(sum(win)==2 & win==1, 0.5, win) # dead-heat adj
      ) %>% ungroup() %>%
      mutate(bsp_bin=gsub('0-2.5', '1.01-2.5', bsp_bin),
             bsp_index_all=1,
             pick=ifelse(winprob_rank %in% ranks, 1, 0),
             winprice=1/winprob_adj, # model price
           #  winprice_sim=1/winprob_sim_adj, # sim model price
             ave_win_bsp=ifelse(win==1, betfairwinsp, NA),
             profit=win*betfairwinsp-1,
             bf_prob = 1/(betfairwinsp),
             exp_value = (betfairwinsp - winprice)/winprice,
             conf_level = winprob_adj - bf_prob # FIXED: was `winprob` (raw, un-normalised
             # within the race) compared against `bf_prob` (market-implied, race-normalised
             # by construction). Comparing those two was a scale mismatch. winprob_adj is
             # the race-normalised model probability, so this now compares like with like.
      ) %>% unique()
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
  
  
  
  ###############################################################################
  # INITIALIZATION
  ###############################################################################
  
  if(savedObjPreds == 0){
  
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
  
  # FIXED: was length(levels(train_data$finish_group)), which always read the
  # multiclass target regardless of modelType, so a binary run was misclassified
  # as multiclass -- this broke both the ROC AUC branch below and (in tuning mode)
  # which metric got used to select the best hyperparameters.
  n_classes  <- length(levels(train_data[[target_var]]))
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
    # Interactions between numeric features (pairwise)
    # step_interact(
    #   terms = ~ pace:distance_m + pace:draw_relative + distance_m:draw_relative
    # ) %>%
    # Remove zero variance predictors
    step_zv(all_predictors()) %>%
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors()) %>%
    # Remove highly correlated predictors
    step_corr(all_numeric_predictors(), threshold = 0.9) %>%
    # SMOTE on Win and Place classes
    step_smote(target_var, over_ratio = 0.9, skip = TRUE) 


  # predictors
  predictors = summary(recipe_ml)
  
  # test sample
  test_data <- model_data %>% 
    filter(meetingdate >= test_date_start,
           meetingdate < test_date_end
    )
  

  print(paste0('test_perc_split = ', round(nrow(test_data)/nrow(train_data)*100, 1),'%'))
  
  
  if(tuneModels == 1) {
    
    print('=== TUNING MODE ===')
    
    pos_weight <- sum(train_data[[target_var]] == "Not_Win") / sum(train_data[[target_var]] == "Win")
    print(paste("scale_pos_weight:", round(pos_weight, 2)))
    
    tune_spec <- boost_tree(
      trees          = tune(),
      min_n          = tune(),
      tree_depth     = tune(),
      learn_rate     = tune(),
      loss_reduction = tune()
    ) %>%
      set_mode('classification') %>%
      set_engine(tree_model, verbose = 1) # , scale_pos_weight = pos_weight
    
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
    
    # set.seed(123)
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
  set.seed(seed_val)
  final_model <- workflow_ml %>% fit(data = train_data)
  
  
  print('Features used:')
  print(names(final_model$pre$mold$predictors))
  
  print('Feature importance:')
  print(vip::vip(final_model, num_features = 20))
  
  } else {
    
    # saved modle obj
    final_model <- readRDS(savedWinprobObj)
    
  }
  
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
  # predictions_sim <- final_model %>%
  #   predict(test_data_sim) %>%
  #   bind_cols(final_model %>% predict(test_data_sim, type = "prob"))
  # 
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
    # FIXED: roc_auc() for a 2-class problem wants ONE probability column (the event
    # level), not both .pred_Win and .pred_Not_Win -- passing both is the multiclass
    # calling convention and throws "using a binary metric but passed multiple columns".
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
  # DIAGNOSTIC: PER-RACE TOP1 ACCURACY vs MARKET FAVOURITE BASELINE
  ###############################################################################
  # Pure diagnostic -- no change to the model, recipe, or training. Global accuracy/
  # confusion-matrix metrics above are evaluated at an arbitrary 0.5 cutoff on a
  # ~91%/9% imbalanced problem, which isn't the natural way to judge a model whose
  # real job is "pick the most likely winner in each race". top1_accuracy asks: of
  # races in the test set, how often was the model's highest-ranked horse the actual
  # winner? fav_top1_accuracy is the same question asked of simply backing the
  # market's own favourite (shortest betfairwinsp) every race -- the baseline the
  # model needs to beat to be adding any value over the market.
  
  if(n_classes == 2) {
    
    print('=== PER-RACE TOP1 ACCURACY (model) ===')
    
    race_eval <- test_data %>%
      select(raceid, all_of(target_var), betfairwinsp) %>%
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
  }
  
  print('=== COMPLETE ===')