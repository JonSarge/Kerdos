#################################################
# KERDOS V5 (consolidated)
# UK Horse Racing Model — single-script pipeline
#
# Merges: uk_racing_data.R + uk_racing_model_tune.R + kerdos_v4.R
# into one script with explicit config, train-only imputation,
# and functions that take arguments instead of reading globals.
#
# Sections:
#   0. Config
#   1. Libraries / helpers
#   2. Load source data
#   3. Feature engineering
#   4. Ratings reconciliation (Kerdos vs Timeform) — TRAIN-ONLY FIT
#   5. Pre-model dataframe (joins: jockey/trainer/flucs/class)
#   6. Model dataframe + filters (KERDOS V4 rules)
#   7. Train/test split
#   8. Model spec, recipe, tuning/fit (as functions)
#   9. Evaluation (metrics, confusion matrix, top1 vs favourite baseline)
#  10. PnL functions + report
#  11. Run pipeline
#
# To Do:
#  - jockey/trainer ratings refresh cadence
#  - daily price fluctuation features
#################################################

#### 0. CONFIG ##################################################

cfg <- list(
  # data window pulled from DB
  db_query_from       = '2024-01-01',
  
  # train/test split
  train_date_start    = '2024-01-01',
  test_date_start     = '2026-01-01',
  test_date_end       = '2026-12-31',
  
  # model
  model_type           = 'binary',        # 'binary' or 'multinomial'
  model_vars           = c(
    'kerdos_rating_rank', 'fave_price', 'bsp_prob', 'bsp_rank',
    'category_rank', 'category_blended_score', 'draw_relative',
    'distance_m', 'weightcarried', 'horseage', 'daysoff'
  ),
  race_filters         = list(
    surface   = 'Turf',
    class_in  = 2:6,
    code_excl = c('S', 'W', 'GA', 'IM')
  ),
  train_bsp_bounds     = c(1.2, 50),
  test_bsp_max         = 100,
  
  # xgboost tuning grid (single combo unless you expand these)
  trees                = 500,
  min_n                = 10,
  tree_depth           = 3,
  learn_rate           = 0.1,
  loss_reduction       = 0.01,
  tune_models          = TRUE,
  seed                 = 20260722,
  
  # pnl reporting
  conf_levels          = c(0, 0.025, 0.075),
  
  # bsp bucket breaks
  bsp_int              = c(seq(0, 10, 2.5), 15, 25, 50, 1001),
  
  # output paths
  jockey_ratings_path  = 'jockey_ratings.csv',
  trainer_ratings_path = 'trainer_ratings.csv',
  class_rankings_path  = 'gb_racing_raceclass.csv',
  flucs_data_path      = 'kerdos_flucs_data.rda',
  model_out_path       = sprintf('kerdos_model_%s.rds', format(Sys.Date(), '%Y%m%d'))
)

cfg$target_var <- if (cfg$model_type == 'binary') 'finish_group_binary' else 'finish_group'

#### 1. LIBRARIES / HELPERS #####################################

library(dplyr)
library(stringr)
library(tidymodels)
library(tidyverse)
library(vip)
library(probably)
library(themis)
library(doParallel)
library(rpart)
library(rpart.plot)
library(glmnet)
library(RMariaDB)
library(slider)
library(bonsai)
library(yardstick)

ch  <- function(x) as.character(x)
fct <- function(x) as.factor(x)
nm  <- function(x) as.numeric(x)
hms_times <- function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), '%H:%M')

options(dplyr.summarise.inform = FALSE)
options(scipen = 99)
set.seed(cfg$seed)

# parallel cluster — always torn down via on.exit() in run_pipeline()
setup_cluster <- function() {
  cl <- makePSOCKcluster(max(1, parallel::detectCores() - 1))
  registerDoParallel(cl)
  cl
}
teardown_cluster <- function(cl) {
  tryCatch({
    stopCluster(cl)
    registerDoSEQ()
  }, error = function(e) invisible(NULL))
}

#### 2. LOAD SOURCE DATA #########################################

# Credentials come from environment variables — NEVER hard-code these.
# Set via .Renviron or your secrets manager:
#   KERDOS_DB_USER, KERDOS_DB_PASSWORD, KERDOS_DB_NAME, KERDOS_DB_HOST, KERDOS_DB_PORT
load_source_data <- function(query_from) {
  required_env <- c('KERDOS_DB_USER', 'KERDOS_DB_PASSWORD', 'KERDOS_DB_NAME',
                    'KERDOS_DB_HOST', 'KERDOS_DB_PORT')
  missing_env <- required_env[Sys.getenv(required_env) == '']
  if (length(missing_env) > 0) {
    stop('Missing DB env vars: ', paste(missing_env, collapse = ', '))
  }
  
  kon <- dbConnect(
    MariaDB(),
    user     = Sys.getenv('KERDOS_DB_USER'),
    password = Sys.getenv('KERDOS_DB_PASSWORD'),
    dbname   = Sys.getenv('KERDOS_DB_NAME'),
    host     = Sys.getenv('KERDOS_DB_HOST'),
    port     = as.integer(Sys.getenv('KERDOS_DB_PORT'))
  )
  on.exit(dbDisconnect(kon), add = TRUE)
  
  gb_racing_sql <- dbGetQuery(
    kon,
    sprintf("select distinct * from gb_racing_data where scheduledtimeofracegmt >= '%s'", query_from)
  )
  
  jockey_ratings  <- read.csv(cfg$jockey_ratings_path)
  trainer_ratings <- read.csv(cfg$trainer_ratings_path)
  class_rankings  <- read.csv(cfg$class_rankings_path)
  load(cfg$flucs_data_path)  # provides flucs_data_csv
  
  list(
    gb_racing_sql   = gb_racing_sql,
    jockey_ratings  = jockey_ratings,
    trainer_ratings = trainer_ratings,
    class_rankings  = class_rankings,
    flucs_data_csv  = flucs_data_csv
  )
}

#### 3. FEATURE ENGINEERING ######################################

engineer_features <- function(gb_racing_sql) {
  
  bsp_int <- cfg$bsp_int
  
  gb_flat_all_cols <- gb_racing_sql %>%
    filter(
      countrycode == 'GBR',
      racetype == 'Flat',
      positionofficial > 0            # remove DNF
    ) %>%
    mutate(
      meetingdate = as.Date(scheduledtimeofracegmt),
      year  = year(meetingdate),
      month = month(meetingdate, label = TRUE, abbr = TRUE),
      Period = case_when(
        meetingdate >= "2023-07-01" & meetingdate <= "2023-12-31" ~ 0,
        meetingdate >= "2024-01-01" & meetingdate <= "2024-06-30" ~ 1,
        meetingdate >= "2024-07-01" & meetingdate <= "2024-12-31" ~ 2,
        meetingdate >= "2025-01-01" & meetingdate <= "2025-06-30" ~ 3,
        meetingdate >= "2025-07-01" & meetingdate <= "2025-12-31" ~ 4,
        meetingdate >= "2026-01-01" & meetingdate <= "2026-06-30" ~ 5,
        meetingdate >= "2026-07-01" & meetingdate <= "2026-12-31" ~ 6,
        TRUE ~ NA_real_
      ),
      finish_group = case_when(
        positionofficial == 1 ~ "Win",
        positionofficial %in% 2:3 ~ "Place",
        positionofficial %in% 4:6 ~ "Unplaced",
        TRUE ~ "Out_of_Money"
      ),
      finish_group = factor(finish_group, levels = c("Win", "Place", "Unplaced", "Out_of_Money")),
      finish_group_binary = factor(
        ifelse(positionofficial == 1, "Win", "Not_Win"),
        levels = c("Win", "Not_Win")
      ),
      field_size = case_when(
        numberofrunners < 6 ~ 'Small',
        between(numberofrunners, 6, 10) ~ 'Medium',
        between(numberofrunners, 11, 16) ~ 'Large',
        TRUE ~ 'Very Large'
      ),
      draw_relative = (draw - 1) / (numberofrunners - 1),
      draw_bin = case_when(
        draw_relative <= 0.1 ~ 'Rail',
        draw_relative > 0.1  & draw_relative <= 0.33 ~ 'Inside',
        draw_relative > 0.33 & draw_relative <= 0.67 ~ 'Middle',
        draw_relative > 0.67 & draw_relative <= 0.9  ~ 'Outside',
        TRUE ~ 'Wide'
      ),
      raceclass_pred = case_when(
        racecode %in% c('G1', 'G2', 'G3', 'L') ~ 1,
        ratinglimitupper > 105 ~ 1,
        between(ratinglimitupper, 100, 105) ~ 2,
        between(ratinglimitupper, 86, 99)   ~ 3,
        between(ratinglimitupper, 76, 85)   ~ 4,
        between(ratinglimitupper, 66, 75)   ~ 5,
        between(ratinglimitupper, 45, 65)   ~ 6,
        TRUE ~ raceclass
      ),
      raceclass = ifelse(is.na(raceclass), raceclass_pred, raceclass),
      class_prior = case_when(
        raceclass > run1_raceclass ~ 'Down',
        raceclass < run1_raceclass ~ 'Up',
        raceclass == run1_raceclass ~ 'Same',
        TRUE ~ NA
      ),
      weight_relative = scale(weightcarried)[, 1],
      distance_m = distance * 201.168,
      daysoff = ifelse(is.na(daysoff), 0, daysoff),
      pace_pred = nm(str_extract(pacemapdata, '\\d+$')),
      win = ifelse(positionofficial == 1, 1, 0),
      win_place = ifelse(positionofficial %in% c(1, 2, 3), 1, 0),
      profit = win * betfairwinsp - 1,
      bsp_index = cut(betfairwinsp, bsp_int, right = FALSE, labels = FALSE),
      bsp_bin = gsub(',', '-', gsub('\\)', '\\]', cut(betfairwinsp, bsp_int, right = FALSE))),
      bsp_log = log(betfairwinsp + 1),
      bsp_prob = 1 / betfairwinsp,
      bsp_prob_1 = bsp_prob^0.5,
      run1_bsp_prob = 1 / run1_betfairwinsp,
      run1_betfairwinsp = ifelse(is.na(run1_betfairwinsp) | run1_betfairwinsp == 0,
                                 betfairwinsp, run1_betfairwinsp),
      bsp_run1_fluc = (run1_betfairwinsp - betfairwinsp) / run1_betfairwinsp,
      bsp_run1_fluc = ifelse(bsp_run1_fluc < -2, -2, bsp_run1_fluc),
      bsp_prob_run1_fluc = (bsp_prob - run1_bsp_prob) / run1_bsp_prob,
      bsp_prob_run1_fluc = ifelse(is.na(bsp_prob_run1_fluc), 0, bsp_prob_run1_fluc)
    ) %>%
    group_by(raceid) %>%
    mutate(fave_price = min(ispdecimal)) %>%
    ungroup() %>%
    arrange(scheduledtimeofracegmt)
  
  dupes <- gb_flat_all_cols %>%
    group_by(raceid, horsecode) %>%
    filter(n() > 1) %>%
    ungroup()
  if (nrow(dupes) > 0) warning('CHECK RUNNER DUPES: ', nrow(dupes), ' duplicate raceid/horsecode rows found')
  
  gb_flat_all_cols
}

#### 4. RATINGS RECONCILIATION — TRAIN-ONLY FIT ##################
#
# FIX vs original: kerdos_est_model was previously fit on the FULL dataset
# (including the 2026 test window), then used to impute kerdos_rating for
# every row, including test rows. That's leakage — the test-period
# Kerdos/Timeform relationship shouldn't inform imputation used to build
# test-period features. Here the model is fit ONLY on rows dated before
# cfg$test_date_start, then applied to the full dataset.

race_vars <- c('raceid', 'racenumber', 'meetingdate', 'scheduledtimeofracegmt', 'coursename',
               'horsecode', 'horsename', 'positionofficial', 'numberofrunners', 'betfairwinsp',
               'bsp_index', 'bsp_bin', 'raceclass', 'run1_raceclass', 'racecode', 'racesurfacename')

fit_kerdos_est_model <- function(gb_flat_all_cols, test_date_start) {
  
  rating_anomalies <- gb_flat_all_cols %>%
    mutate(rating_diff_perc = (pre_race_rating_adjusted - preraceadjustedrating) / preraceadjustedrating) %>%
    filter(abs(rating_diff_perc) > 0.5 | preraceadjustedrating %in% c(0, 999)) %>%
    pull(raceid)
  
  rating_dat <- gb_flat_all_cols %>%
    filter(!raceid %in% rating_anomalies) %>%
    select(all_of(race_vars), betfairwinsp, positionofficial, pre_race_rating_raw,
           contains(c('rating', 'figure', 'trw', 'Rating')))
  
  kerdos_ratings <- rating_dat %>%
    select(all_of(race_vars), pre_race_rating_adjusted, contains('post_race'),
           pre_race_rating_raw, contains('postrace'), -ends_with('_symbol')) %>%
    unique()
  tf_ratings <- rating_dat %>%
    select(all_of(race_vars), contains(c('adjustedrating', 'masterrating', 'performancerating', 'timefigure'))) %>%
    unique()
  
  both_ratings <- kerdos_ratings %>%
    select(-starts_with('run')) %>%
    left_join(
      tf_ratings %>% select(raceid, horsecode, preraceadjustedrating:performancerating, timefigure),
      by = c('raceid', 'horsecode')
    ) %>%
    filter(complete.cases(.), pre_race_rating_adjusted != 0)
  
  # TRAIN-ONLY slice for fitting the imputation model
  both_ratings_train <- both_ratings %>% filter(meetingdate < test_date_start)
  
  kerdos_est_model <- lm(pre_race_rating_adjusted ~ preraceadjustedrating, data = both_ratings_train)
  
  list(model = kerdos_est_model, both_ratings = both_ratings)
}

#### 5. PRE-MODEL DATAFRAME ######################################

build_pre_model_data <- function(gb_flat_all_cols, kerdos_est_model,
                                 jockey_ratings, trainer_ratings,
                                 class_rankings, flucs_data_csv) {
  
  factor_vars <- c('racenumber', 'draw', 'draw_bin', 'positionofficial', 'horsegender', 'distancetype',
                   'racetype', 'raceclass', 'racesurfacename', 'racecode', 'going',
                   'iphintsoverallpace', 'pacemapdata', 'field_size', 'jockeycode', 'jockeyfullname',
                   'trainercode', 'trainerfullname', 'ratingstars', 'rating123', 'significantimprover',
                   'jockeyinform', 'toprated', 'hottrainer', 'eligibilitysexlimit',
                   'preracemastersymbol', 'hotrace', 'pre_race_rating_symbol_for_16')
  
  na_zero_cols_df <- gb_flat_all_cols %>%
    select(raceid, scheduledtimeofracegmt, coursename, where(~ all(. == 0, na.rm = TRUE)))
  na_cols   <- na_zero_cols_df %>% summarise_all(~ all(is.na(.))) %>% select_if(~.) %>% names()
  zero_cols <- na_zero_cols_df %>% summarise_all(~ all(. == 0)) %>% select_if(~.) %>% names()
  
  jockey_rating_mins <- jockey_ratings %>%
    group_by(jockeyfullname = Jockey) %>%
    summarise(Rating_Jockey_min = min(RATING)) %>%
    ungroup()
  
  gb_flat_all_cols %>%
    mutate(across(all_of(factor_vars), ~ fct(.))) %>%
    mutate(
      kerdos_rating_pred = predict(kerdos_est_model, .),
      kerdos_rating = case_when(
        is.na(pre_race_rating_adjusted) & !preraceadjustedrating %in% c(0, 999) ~ kerdos_rating_pred,
        pre_race_rating_adjusted < 20 & between(preraceadjustedrating, 40, 200) ~ kerdos_rating_pred,
        TRUE ~ pre_race_rating_adjusted
      )
    ) %>%
    group_by(raceid) %>%
    mutate(
      kerdos_rating_z   = nm(scale(kerdos_rating)),
      kerdos_rating_adj = pnorm(kerdos_rating_z) * 100,
      kerdos_rating_rank = rank(-kerdos_rating_z, ties.method = "min"),
      bsp_rank = rank(betfairwinsp, ties.method = "min") / numberofrunners
    ) %>%
    ungroup() %>%
    relocate(contains('kerdos_rating'), .after = pre_race_rating_adjusted) %>%
    select(!c(all_of(na_cols), all_of(zero_cols))) %>%
    left_join(jockey_ratings %>% select(jockeyfullname = Jockey, Period, Rating = RATING),
              by = c('jockeyfullname', 'Period')) %>%
    left_join(trainer_ratings %>% select(trainerfullname = Trainer, Period, Rating = OVERALL),
              by = c('trainerfullname', 'Period'), suffix = c('_Jockey', '_Trainer')) %>%
    left_join(jockey_rating_mins, by = 'jockeyfullname') %>%
    mutate(
      jockey_rating = ifelse(is.na(Rating_Jockey), Rating_Jockey_min, Rating_Jockey),
      jockey_rating_na = as.factor(ifelse(is.na(Rating_Jockey), 1, 0))
    ) %>%
    left_join(
      flucs_data_csv %>% select(raceid, horsecode, bsp_60, bsp_10, bsp_5) %>% mutate(horsecode = nm(horsecode)),
      by = c('raceid', 'horsecode')
    ) %>%
    left_join(class_rankings, by = 'raceid') %>%
    arrange(scheduledtimeofracegmt, saddlecloth)
}

#### 6. MODEL DATAFRAME + FILTERS (KERDOS V4 rules) ##############

build_model_data <- function(pre_model_data, race_filters) {
  pre_model_data %>%
    filter(
      racesurfacename == race_filters$surface,
      raceclass %in% race_filters$class_in,
      !racecode %in% race_filters$code_excl,
      !(preraceadjustedrating >= 70 & kerdos_rating < 30)   # ad hoc rating-mismatch filter — see To Do
    ) %>%
    group_by(raceid) %>%
    mutate(jockey_rating = nm(scale(jockey_rating))) %>%
    ungroup()
}

check_nas <- function(model_data, race_vars, model_vars) {
  na_df <- model_data %>%
    select(where(~ any(is.na(.)))) %>%
    filter(if_any(everything(), is.na))
  
  bad_race_vars  <- race_vars[race_vars %in% names(na_df)]
  bad_model_vars <- model_vars[model_vars %in% names(na_df)]
  
  # FIX vs original: bare string literals here were silent no-ops when
  # sourced non-interactively. Now they actually stop the run.
  if (length(bad_race_vars)  > 0) stop('NAs found in race vars: ',  paste(bad_race_vars, collapse = ', '))
  if (length(bad_model_vars) > 0) stop('NAs found in model vars: ', paste(bad_model_vars, collapse = ', '))
  invisible(TRUE)
}

#### 7. TRAIN/TEST SPLIT ##########################################

split_train_test <- function(model_data, cfg) {
  train_data <- model_data %>%
    filter(
      meetingdate >= cfg$train_date_start,
      meetingdate < cfg$test_date_start,
      between(betfairwinsp, cfg$train_bsp_bounds[1], cfg$train_bsp_bounds[2])
    ) %>%
    select(raceid, meetingdate, all_of(cfg$model_vars), all_of(cfg$target_var))
  
  train_nas <- train_data %>% filter(!complete.cases(.))
  if (nrow(train_nas) > 0) stop('NAs remain in train_data after filtering: ', nrow(train_nas), ' rows')
  
  test_data <- model_data %>%
    filter(meetingdate >= cfg$test_date_start, meetingdate < cfg$test_date_end)
  
  list(train_data = train_data, test_data = test_data)
}

#### 8. MODEL SPEC / RECIPE / TUNE / FIT #########################

build_recipe <- function(model_formula, train_data, target_var) {
  recipe(model_formula, data = train_data) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_corr(all_numeric_predictors(), threshold = 0.9) %>%
    step_smote(all_of(target_var), over_ratio = 0.5, skip = TRUE)
}

fit_model <- function(train_data, target_var, cfg) {
  
  n_classes   <- length(levels(train_data[[target_var]]))
  best_metric <- if (n_classes > 2) 'accuracy' else 'roc_auc'
  tune_metrics <- if (n_classes > 2) metric_set(accuracy) else metric_set(accuracy, roc_auc)
  
  model_formula <- as.formula(paste(target_var, '~', paste(cfg$model_vars, collapse = ' + ')))
  recipe_ml <- build_recipe(model_formula, train_data, target_var)
  
  folds_ml <- sliding_period(
    train_data, index = meetingdate, period = "month",
    lookback = Inf, assess_stop = 1
  )
  
  set.seed(cfg$seed)
  
  if (isTRUE(cfg$tune_models)) {
    tune_spec <- boost_tree(
      trees = tune(), min_n = tune(), tree_depth = tune(),
      learn_rate = tune(), loss_reduction = tune()
    ) %>% set_mode('classification') %>% set_engine('xgboost', verbose = 1)
    
    tree_grid <- expand.grid(
      trees = cfg$trees, min_n = cfg$min_n, tree_depth = cfg$tree_depth,
      learn_rate = cfg$learn_rate, loss_reduction = cfg$loss_reduction
    ) %>% unique()
    
    workflow_ml <- workflow() %>%
      add_recipe(recipe_ml, blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)) %>%
      add_model(tune_spec)
    
    tune_results <- tune_grid(
      workflow_ml, resamples = folds_ml, grid = tree_grid,
      metrics = tune_metrics,
      control = control_grid(verbose = TRUE, save_pred = TRUE, allow_par = TRUE)
    )
    
    final_params <- tune_results %>% select_best(metric = best_metric)
    
  } else {
    tune_spec <- boost_tree(
      trees = cfg$trees, min_n = cfg$min_n, tree_depth = cfg$tree_depth,
      learn_rate = cfg$learn_rate, loss_reduction = cfg$loss_reduction
    ) %>% set_mode('classification') %>% set_engine('xgboost', verbose = 1)
    
    workflow_ml <- workflow() %>%
      add_recipe(recipe_ml, blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)) %>%
      add_model(tune_spec)
    
    tune_results <- fit_resamples(
      workflow_ml, resamples = folds_ml, metrics = tune_metrics,
      control = control_resamples(verbose = TRUE, save_pred = TRUE, allow_par = TRUE)
    )
    
    final_params <- tibble(
      trees = cfg$trees, min_n = cfg$min_n, tree_depth = cfg$tree_depth,
      learn_rate = cfg$learn_rate, loss_reduction = cfg$loss_reduction
    )
  }
  
  if (nrow(collect_metrics(tune_results)) == 0) stop('Model fitting failed: no CV metrics returned')
  
  workflow_ml <- workflow_ml %>% finalize_workflow(final_params)
  final_model <- workflow_ml %>% fit(data = train_data)
  
  list(
    model         = final_model,
    final_params  = final_params,
    cv_metrics    = collect_metrics(tune_results),
    n_classes     = n_classes
  )
}

#### 9. EVALUATION ################################################

price_sim <- function(test_data, price_col) {
  test_data %>%
    rename(price = !!sym(price_col)) %>%
    group_by(raceid) %>%
    mutate(
      total_prob      = sum(1 / price),
      is_fav          = price == min(price),
      fav_price       = nm(price[is_fav][1]),
      new_fav_price   = fav_price + runif(1, -0.1, 0.1),
      new_fav_prob    = 1 / new_fav_price,
      nonfav_prob_orig = sum((1 / price)[!is_fav]),
      remaining_prob  = total_prob - new_fav_prob,
      scale_factor    = remaining_prob / nonfav_prob_orig,
      new_prob        = if_else(is_fav, new_fav_prob, (1 / price) * scale_factor),
      betfairwinsp_sim = 1 / new_prob
    ) %>%
    ungroup() %>%
    select(betfairwinsp_sim)
}

generate_predictions <- function(final_model, test_data, target_var) {
  predictions <- final_model %>%
    predict(test_data) %>%
    bind_cols(final_model %>% predict(test_data, type = "prob")) %>%
    bind_cols(test_data %>% select(all_of(target_var))) %>%
    rowwise() %>%
    mutate(max_pred_prob = max(c_across(starts_with(".pred_") & where(is.numeric)))) %>%
    ungroup()
  
  test_data_sim <- test_data %>%
    bind_cols(price_sim(test_data, 'betfairwinsp')) %>%
    group_by(raceid) %>%
    mutate(
      bsp_prob = 1 / betfairwinsp_sim,
      bsp_rank = rank(betfairwinsp_sim, ties.method = "min") / numberofrunners,
      bsp_prob_run1_fluc = (bsp_prob - run1_bsp_prob) / run1_bsp_prob,
      bsp_prob_run1_fluc = ifelse(is.na(bsp_prob_run1_fluc), 0, bsp_prob_run1_fluc)
    ) %>%
    ungroup()
  
  predictions_sim <- final_model %>%
    predict(test_data_sim) %>%
    bind_cols(final_model %>% predict(test_data_sim, type = "prob"))
  
  list(predictions = predictions, test_data_sim = test_data_sim, predictions_sim = predictions_sim)
}

evaluate_model <- function(predictions, target_var, n_classes) {
  cat('=== PERFORMANCE METRICS ===\n')
  print(predictions %>% metrics(truth = !!sym(target_var), estimate = .pred_class))
  
  cat('=== CONFUSION MATRIX ===\n')
  print(predictions %>% conf_mat(truth = !!sym(target_var), estimate = .pred_class))
  
  cat('=== PER-CLASS ACCURACY ===\n')
  predictions %>%
    group_by(.data[[target_var]]) %>%
    summarise(n = n(), accuracy = mean(.pred_class == .data[[target_var]]), mean_prob = mean(max_pred_prob)) %>%
    arrange(desc(accuracy)) %>%
    print()
  
  cat('=== ROC AUC ===\n')
  if (n_classes == 2) {
    event_col <- paste0('.pred_', levels(predictions[[target_var]])[1])
    roc_val <- predictions %>% roc_auc(truth = !!sym(target_var), !!sym(event_col))
    cat('AUC:', round(roc_val$.estimate, 4), '\n')
  } else {
    prob_cols <- paste0('.pred_', levels(predictions[[target_var]]))
    existing_cols <- prob_cols[prob_cols %in% names(predictions)]
    if (length(existing_cols) == n_classes) {
      print(predictions %>% roc_auc(truth = !!sym(target_var), all_of(existing_cols)))
    } else {
      cat('WARNING: mismatch in probability columns, skipping ROC\n')
    }
  }
}

top1_vs_favourite <- function(test_data, predictions, target_var, n_classes) {
  if (n_classes != 2) return(invisible(NULL))
  
  race_eval <- test_data %>%
    select(raceid, all_of(target_var), betfairwinsp) %>%
    bind_cols(winprob = predictions$.pred_Win) %>%
    group_by(raceid) %>%
    mutate(winprob_rank = dense_rank(-winprob), actual_winner = .data[[target_var]] == "Win") %>%
    ungroup()
  
  top1_summary <- race_eval %>%
    group_by(raceid) %>%
    summarise(correct_top1 = any(winprob_rank == 1 & actual_winner), has_winner = any(actual_winner)) %>%
    filter(has_winner) %>%
    summarise(n_races = n(), top1_accuracy = mean(correct_top1))
  
  fav_baseline <- test_data %>%
    group_by(raceid) %>%
    mutate(fav_rank = dense_rank(betfairwinsp)) %>%
    ungroup() %>%
    group_by(raceid) %>%
    summarise(fav_wins = any(fav_rank == 1 & .data[[target_var]] == "Win")) %>%
    summarise(n_races = n(), fav_top1_accuracy = mean(fav_wins))
  
  cat('=== PER-RACE TOP1 ACCURACY (model) ===\n'); print(top1_summary)
  cat('=== MARKET FAVOURITE BASELINE (top1 accuracy) ===\n'); print(fav_baseline)
  cat(sprintf('Model top1_accuracy (%.3f) vs favourite baseline (%.3f)\n',
              top1_summary$top1_accuracy, fav_baseline$fav_top1_accuracy))
  
  list(top1_summary = top1_summary, fav_baseline = fav_baseline)
}

#### 10. PNL ######################################################

build_pnl_df <- function(test_data, predictions, predictions_sim, test_data_sim, ranks) {
  bind_cols(
    test_data,
    winprob = predictions$.pred_Win,
    winclass = predictions$.pred_class,
    winprob_sim = predictions_sim$.pred_Win,
    betfairwinsp_sim = test_data_sim$betfairwinsp_sim
  ) %>%
    group_by(raceid) %>%
    mutate(
      winprob_adj = winprob / sum(winprob),
      winprob_sim_adj = winprob_sim / sum(winprob_sim),
      winprob_rank = dense_rank(-winprob_adj),
      win = ifelse(positionofficial == 1, 1, 0),
      win = ifelse(sum(win) == 2 & win == 1, 0.5, win)   # dead-heat adj
    ) %>%
    ungroup() %>%
    mutate(
      bsp_bin = gsub('0-2.5', '1.01-2.5', bsp_bin),
      bsp_index_all = 1,
      pick = ifelse(winprob_rank %in% ranks, 1, 0),
      winprice = 1 / winprob_adj,
      winprice_sim = 1 / winprob_sim_adj,
      ave_win_bsp = ifelse(win == 1, betfairwinsp, NA),
      profit = win * betfairwinsp - 1,
      bf_prob = 1 / betfairwinsp,
      exp_value = (betfairwinsp - winprice) / winprice,
      conf_level = winprob_adj - bf_prob   # race-normalised model prob vs market-implied prob
    )
}

pnl_table <- function(dat, vars, val, conf, prob) {
  dat %>%
    filter(pick == 1, exp_value > val, conf_level > conf, winprob_adj > prob) %>%
    group_by(across(all_of(vars))) %>%
    summarise(
      bets = sum(pick),
      sr = mean(win),
      ave_win_price = mean(na.omit(ave_win_bsp)),
      profit = sum(profit),
      roi = sum(profit) / sum(pick)
    )
}

run_pnl_report <- function(test_pnl_dat, cfg) {
  for (i in seq_along(cfg$conf_levels)) {
    mins <- c(0, cfg$conf_levels[i], 0.1)   # exp val; conf level; winprob min
    print(cbind(
      pnl_table(test_pnl_dat, c('surface', 'bsp_index_all'), mins[1], mins[2], mins[3]),
      conf = cfg$conf_levels[i]
    ))
  }
  print(pnl_table(test_pnl_dat, c('surface', 'month'), 0, 0, 0.1))
  print(pnl_table(test_pnl_dat, c('surface', 'bsp_index', 'bsp_bin'), 0, 0, 0.1))
  print(pnl_table(test_pnl_dat, c('surface', 'bsp_index_all'), 0, 0, 0.1))
  print(pnl_table(test_pnl_dat, c('surface', 'winprob_rank'), 0, 0, 0.1))
  print(pnl_table(test_pnl_dat, c('surface', 'raceclass'), 0, 0, 0.1))
  print(pnl_table(test_pnl_dat, c('surface', 'racecode'), 0, 0, 0.1))
}

#### 11. RUN PIPELINE #############################################

run_pipeline <- function(cfg) {
  
  cl <- setup_cluster()
  on.exit(teardown_cluster(cl), add = TRUE)
  
  cat('Loading source data...\n')
  src <- load_source_data(cfg$db_query_from)
  
  cat('Engineering features...\n')
  gb_flat_all_cols <- engineer_features(src$gb_racing_sql)
  
  cat('Fitting Kerdos/Timeform reconciliation model (train period only)...\n')
  rating_fit <- fit_kerdos_est_model(gb_flat_all_cols, cfg$test_date_start)
  
  cat('Building pre-model data...\n')
  pre_model_data <- build_pre_model_data(
    gb_flat_all_cols, rating_fit$model,
    src$jockey_ratings, src$trainer_ratings, src$class_rankings, src$flucs_data_csv
  )
  
  cat('Applying model filters...\n')
  model_data <- build_model_data(pre_model_data, cfg$race_filters)
  check_nas(model_data, race_vars, cfg$model_vars)
  
  cat('Splitting train/test...\n')
  split <- split_train_test(model_data, cfg)
  cat(sprintf('test_perc_split = %.1f%%\n', nrow(split$test_data) / nrow(split$train_data) * 100))
  
  cat('Tuning/fitting model...\n')
  fit_out <- fit_model(split$train_data, cfg$target_var, cfg)
  cat('Best parameters:\n'); print(fit_out$final_params)
  cat('CV results:\n'); print(fit_out$cv_metrics)
  cat('Features used:\n'); print(names(fit_out$model$pre$mold$predictors))
  
  cat('Generating predictions...\n')
  preds <- generate_predictions(fit_out$model, split$test_data, cfg$target_var)
  
  evaluate_model(preds$predictions, cfg$target_var, fit_out$n_classes)
  top1_vs_favourite(split$test_data, preds$predictions, cfg$target_var, fit_out$n_classes)
  
  cat('Building PnL report...\n')
  test_pnl_dat <- build_pnl_df(split$test_data, preds$predictions, preds$predictions_sim,
                               preds$test_data_sim, ranks = 1:5) %>%
    select(all_of(race_vars), month, all_of(cfg$model_vars), win:conf_level,
           Rating_Jockey, Rating_Trainer, -winclass) %>%
    relocate(starts_with('betfairwin'), .before = winprob) %>%
    rename(surface = racesurfacename) %>%
    filter(betfairwinsp <= cfg$test_bsp_max)
  
  test_data_nas <- test_pnl_dat %>%
    select(-c(ave_win_bsp, run1_raceclass, starts_with('Rating_'))) %>%
    filter(!complete.cases(.))
  if (nrow(test_data_nas) > 0) warning(nrow(test_data_nas), ' rows with NAs in test_pnl_dat')
  
  run_pnl_report(test_pnl_dat, cfg)
  
  # Persist the run so V5 vs V6 etc. can be compared later instead of relying on console scrollback
  saveRDS(
    list(
      model = fit_out$model, final_params = fit_out$final_params, cv_metrics = fit_out$cv_metrics,
      cfg = cfg, timestamp = Sys.time()
    ),
    file = cfg$model_out_path
  )
  cat('Saved model + run metadata to', cfg$model_out_path, '\n')
  
  list(fit_out = fit_out, preds = preds, test_pnl_dat = test_pnl_dat, split = split)
}

# Entry point
if (sys.nframe() == 0) {
  results <- run_pipeline(cfg)
}