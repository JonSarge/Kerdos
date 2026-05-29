###############################################################################
# Horse Racing Classification - ML Finish Group Prediction
###############################################################################

suppressPackageStartupMessages(library(tidymodels))
library(bonsai)
library(yardstick)
library(vip)

###############################################################################
# INITIALIZATION
###############################################################################

tree_model = TreeModel[t]  

print(paste0('Model:', tree_model, ' | Tuning: ', ifelse(tuneModels == 1, 'YES', 'NO')) )

folds_ml <- vfold_cv(train_data, v = 2, strata = finish_group)
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
  # Normalize numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  # Remove zero variance predictors
  step_zv(all_predictors()) %>%
  # Remove highly correlated predictors
  step_corr(all_numeric_predictors(), threshold = 0.9)

# predictors
predictors = summary(recipe_ml)

# test sample
test_data <- model_data_final %>% 
  filter(meetingdate >= test_date_start,
         meetingdate < test_date_end
  )

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
  
  tune_results <- tune_grid(
    tune_spec,
    model_formula,
    resamples = folds_ml,
    grid      = tree_grid,
    metrics   = tune_metrics,
    control   = control_grid(verbose = TRUE, save_pred = TRUE, allow_par = FALSE)
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
  

  tune_results <- fit_resamples(
    tune_spec,
    model_formula,
    resamples = folds_ml,
    metrics   = tune_metrics,
    control   = control_resamples(verbose = TRUE, save_pred = TRUE, allow_par = FALSE)
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

workflow_ml <- workflow() %>%
  add_recipe(
    recipe_ml,
    blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
  ) %>%
  add_model(tune_spec) %>%
  finalize_workflow(final_params)

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
  bind_cols(test_data %>% select(finish_group)) %>%
  rowwise() %>%
  mutate(max_pred_prob = max(c_across(starts_with(".pred_") & where(is.numeric)))) %>%
  ungroup()

print('=== PERFORMANCE METRICS ===')
print(predictions %>% metrics(truth = finish_group, estimate = .pred_class))

print('=== CONFUSION MATRIX ===')
print(predictions %>% conf_mat(truth = finish_group, estimate = .pred_class))

print('=== PER-CLASS ACCURACY ===')
predictions %>%
  group_by(finish_group) %>%
  summarise(
    n        = n(),
    accuracy = mean(.pred_class == finish_group),
    mean_prob = mean(max_pred_prob)
  ) %>%
  arrange(desc(accuracy)) %>%
  print()

print('=== ROC AUC ===')
prob_cols <- paste0(".pred_", levels(predictions$finish_group))
existing_cols <- prob_cols[prob_cols %in% names(predictions)]

if(n_classes == 2) {
  roc_val <- predictions %>% roc_auc(truth = finish_group, all_of(existing_cols))
  print(paste("AUC:", round(roc_val$.estimate, 4)))
} else {
  if(length(existing_cols) == n_classes) {
    print(predictions %>% roc_auc(truth = finish_group, all_of(existing_cols)))
  } else {
    print("WARNING: Mismatch in probability columns, skipping ROC")
  }
}

print('=== COMPLETE ===')