###############################################################################
# RANKING MODEL - rank:pairwise with raceid as group
###############################################################################
library(xgboost)

# --- Prep features using the SAME recipe (no SMOTE, since ranking doesn't
# use a binary label the way SMOTE assumes -- relevance labels below instead) ---
recipe_prep <- recipe_ml %>% 
  step_rm(all_of(setdiff(character(0), character(0)))) # no-op placeholder if you need to drop steps

# If your recipe_ml still has step_smote() in it, strip it before prepping,
# since SMOTE has no meaning for a ranking label:
recipe_rank <- recipe(model_formula, data = train_data) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9)

prepped <- prep(recipe_rank, training = train_data)

train_baked <- bake(prepped, new_data = train_data)
test_baked  <- bake(prepped, new_data = test_data)

# --- Relevance label: 1 = Win, 0 = Not_Win. (If you later want richer relevance,
# e.g. reward placing 2nd/3rd more than losing outright, this is where finish
# position could be mapped to a graded score instead of binary.) ---
train_label <- as.numeric(train_data[[target_var]] == "Win")
test_label  <- as.numeric(test_data[[target_var]] == "Win")

# --- CRITICAL: sort everything by raceid so group blocks are contiguous ---
train_order <- order(train_data$raceid)
test_order  <- order(test_data$raceid)

train_baked <- train_baked[train_order, ]
train_label <- train_label[train_order]
train_raceid <- train_data$raceid[train_order]

test_baked  <- test_baked[test_order, ]
test_label  <- test_label[test_order]
test_raceid <- test_data$raceid[test_order]

# group sizes = number of runners per race, IN THE SORTED ORDER
train_group <- as.integer(table(train_raceid)[as.character(unique(train_raceid))])
test_group  <- as.integer(table(test_raceid)[as.character(unique(test_raceid))])

# drop raceid/target columns from the feature matrix if the recipe kept them
feature_cols <- setdiff(names(train_baked), c(target_var, "raceid"))

dtrain <- xgb.DMatrix(
  data  = as.matrix(train_baked[, feature_cols]),
  label = train_label
)
setinfo(dtrain, "group", train_group)

dtest <- xgb.DMatrix(
  data  = as.matrix(test_baked[, feature_cols]),
  label = test_label
)
setinfo(dtest, "group", test_group)

# --- Train ---
params <- list(
  objective   = "rank:pairwise",   # try "rank:ndcg" too -- see note below
  eta         = learn_rate,
  max_depth   = tree_depth,
  min_child_weight = min_n,
  gamma       = loss_reduction,
  eval_metric = "ndcg"
)

rank_model <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = trees,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10
)

# --- Evaluate with YOUR top1_accuracy metric, not xgboost's internal NDCG ---
test_scores <- predict(rank_model, dtest)

rank_eval <- tibble(
  raceid = test_raceid,
  score  = test_scores,
  label  = test_label
) %>%
  group_by(raceid) %>%
  mutate(pred_rank = dense_rank(-score)) %>%
  summarise(correct_top1 = any(pred_rank == 1 & label == 1)) %>%
  summarise(n_races = n(), top1_accuracy = mean(correct_top1))

print(rank_eval)

# attach preds to test data
test_data <- test_data %>% mutate(.row_id = row_number())

test_order  <- order(test_data$raceid)
test_baked  <- test_baked[test_order, ]
test_label  <- test_label[test_order]
test_raceid <- test_data$raceid[test_order]
test_row_id <- test_data$.row_id[test_order]   # <-- add this

rank_probs_df <- tibble(
  .row_id = test_row_id,
  raceid  = test_raceid,
  score   = test_scores
) %>%
  group_by(raceid) %>%
  mutate(winprob = exp(score) / sum(exp(score))) %>%
  ungroup() %>%
  select(.row_id, winprob)

# join back to the ORIGINAL test_data by .row_id -- safe regardless of any sorting
test_data_with_rank <- test_data %>%
  left_join(rank_probs_df, by = ".row_id")
