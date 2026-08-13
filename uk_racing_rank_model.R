###############################################################################
# RANKING MODEL - rank:pairwise with raceid as group
###############################################################################
library(xgboost)

# Recipe for preprocessing
recipe_ml <- recipe(model_formula, data = train_data) %>%
  # Create dummy variables
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # Remove zero variance predictors
  step_zv(all_predictors()) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric_predictors()) %>%
  # Remove highly correlated predictors
  step_corr(all_numeric_predictors(), threshold = 0.9)  %>%
  # SMOTE on Win and Place classes
  step_smote(target_var, over_ratio = 0.5, skip = TRUE) 

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
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_smote(target_var, over_ratio = 0.5, skip = TRUE) 

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

# blend in logit/log-odds space, not raw probability space -- this is what
# bookmakers/quant modelers actually do, since probabilities near 0/1 behave
# badly under linear blending but log-odds blend sensibly
logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))

w <- 1  # weight on the model; (1-w) on market. Tune this via CV on top1_accuracy

test_data_with_rank <- test_data %>%
  left_join(rank_probs_df, by = ".row_id") %>%
  mutate(
    market_prob = 1 / betfairwinsp,       # or bsp_prob_60 if you want T-60 market view
    blended_logit = w * logit(winprob) + (1 - w) * logit(market_prob),
    winprob_blended = inv_logit(blended_logit)
  ) %>%
  group_by(raceid) %>%
  mutate(winprob_blended_adj = winprob_blended / sum(winprob_blended)) %>%
  ungroup()

test_rank_pnl_dat <- test_data_with_rank %>%
  group_by(raceid) %>%
  mutate(
    #winprob_adj=winprob/sum(winprob),
    winprob_rank=dense_rank(-winprob_blended_adj),
    win=ifelse(positionofficial==1, 1, 0),
    win=ifelse(sum(win)==2 & win==1, 0.5, win) # dead-heat adj
  ) %>% ungroup() %>%
  mutate(bsp_bin=gsub('0-2.5', '1.01-2.5', bsp_bin),
         bsp_index_all=1,
         pick=ifelse(winprob_rank %in% c(1:1), 1, 0),
         winprice=1/winprob_blended_adj, # model price
         ave_win_bsp=ifelse(win==1, betfairwinsp, NA),
         profit=win*betfairwinsp-1,
         bf_prob = 1/(betfairwinsp),
         exp_value = (betfairwinsp - winprice)/winprice,
         conf_level = winprob_blended_adj - bf_prob
  )


# pnl summary table
print(test_rank_pnl_dat %>%
    filter(pick==1,
           exp_value > 0, 
           conf_level > 0,
           winprob_blended_adj > 0,
    ) %>%
    group_by(bsp_index_all) %>% # bsp_index, bsp_bin
    summarise(bets=sum(pick), 
              sr=mean(win),  
              ave_win_price=mean(na.omit(ave_win_bsp)),
              profit=sum(profit), 
              roi=sum(profit)/sum(pick)) )

# pnl summary table
print(test_rank_pnl_dat %>%
        filter(pick==1,
               exp_value > 0, 
               conf_level > 0,
               winprob_blended_adj > 0.1,
        ) %>%
        group_by(bsp_index, bsp_bin) %>% # bsp_index, bsp_bin
        summarise(bets=sum(pick), 
                  sr=mean(win),  
                  ave_win_price=mean(na.omit(ave_win_bsp)),
                  profit=sum(profit), 
                  roi=sum(profit)/sum(pick)) )

library(vip)

vip(rank_model, num_features = 20)

# or explicitly specify importance type (gain is the default and most comparable
# to your earlier parsnip/bonsai plots)
print(vip(rank_model, num_features = 20, type = "gain"))

