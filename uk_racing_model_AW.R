#################################################
# KERDOS V4 - v1
# UK Horse Racing Model
#
# commenced: 08.07.2026; updated: 22.07.2026
#
# Actions:
#  - data processing
#  - feature engineering
#  - xgboost models
#  - PnL report (flat-stake) 
#
# To Do:
#  - jockey/trainer ratings
#
#################################################

packages <- c(
  "dplyr", "stringr", "tidymodels", "tidyverse", "vip", "doRNG", "xgboost",
  "probably", "themis", "doParallel", "glmnet", "RMariaDB", "slider"
)

invisible(lapply(packages, function(pkg) {
  if (!(pkg %in% (.packages()))) {
    library(pkg, character.only = TRUE)
  }
}))

# Set up parallel processing
if (!exists("cl") || is.null(cl) || !inherits(cl, "cluster")) {
  cl <- makePSOCKcluster(parallel::detectCores() - 1)
  registerDoParallel(cl)
}
#registerDoRNG(123)   # <-- add this line

seed_val <- as.integer(Sys.time())
registerDoRNG(seed_val)
set.seed(seed_val)

loadNewData = 1

#### import data  preprocess ####

# generate model data
if(!exists('pre_model_data')){
  source('uk_racing_data.R')
}

# declare target var
target_var = 'finish_group_binary'

# Predict finish position using tidymodels
model_vars = c(
  'kerdos_rating_z',
  'kerdos_rating_rank',
  'fave_price',
  'category_blended_score',
  'draw_relative',
  'distance_m',
  'weightcarried',
  'horseage',
  'daysoff',
  #'sb_m',
  'bf_prob_60',
  'raceclass',
  'pace_pred',
  'jockey_rating_top',
  'trainer_rating_top'
  #'bf_prob_run1_fluc'
  #'coursename'
  #'racecode'
  #'bf_prob_60_rank'
  #'bf_prob_60_5'
  #'field_size'
  )


# set market price base in racing_data.R!!!

sim_int_list = list()
sim_n = 1 # no of model sims

seed_val <- as.integer(Sys.time())
registerDoRNG(seed_val)
set.seed(seed_val)

#for(i in 1:length(sim_ints)){
for(j in 1:sim_n){
# modelling df w. final filters    
model_data <- pre_model_data %>%  
  filter(!raceid %in% ratings_remove$raceid, # ADD 1-2 K=NULL RACES BACK IN
         !(preraceadjustedrating >= 70 & kerdos_rating < 30), # fix this in racing data script
         racesurfacename == 'All Weather',
         raceclass %in% c(4:6), # Separate model for C1 races
         !is.na(raceclass),
         !is.na(pace_pred), # FIX THIS!!!
         !is.na(bf_prob_60),
         numberofrunners >= 4
  ) %>% unique()

# model formula
model_formula = as.formula(paste(target_var, '~', paste(model_vars, collapse = " + ")))

# train/test dates
train_date_start = '2025-01-01'
test_date_start = '2026-06-11'   # run from 1st June with training data up to 30th May
test_date_end = '2026-12-31'

# training sample w final filtering
train_data <- model_data %>% 
  filter(meetingdate >= train_date_start,
         meetingdate < test_date_start,
         racecode %in% c('I'), # low quality/sample races    #kerdos_rating_rank %in% c(1:6)
         ) %>%
  select(raceid, meetingdate, all_of(model_vars), all_of(target_var)) # final select

# final check for training data na's
train_data_nas <- train_data %>% filter(!complete.cases(.))

# test data
test_data <- model_data %>% 
  filter(meetingdate >= test_date_start,
         meetingdate <= test_date_end)


#### tune xgboost model ####

# tuning
tuneModels = 0
randomGrid = 1
randomGridSize = 10 

# Create tuning grid with specified values
trees = 500 # specify n trees sequence
min_n = 10 # specify minimum tree n sequenc
tree_depth = 3
learn_rate = 0.1
loss_reduction = 0.01

# run predictions off saved files
savedObjPreds = 0
if(savedObjPreds==1) savedWinprobObj = 'gb_AW_winprobs_2026-10-08.rds' # edit obj name here
source('uk_racing_model_tune.R') # xbg tuning script

#### generate results ####
test_pnl_dat <- pnl_df(c(1:6)) %>%
  select(all_of(race_vars), month, all_of(model_vars), c(win:conf_level), 
         Rating_Jockey, Rating_Trainer, -winclass, goingabbrev) %>%
  relocate(starts_with('betfairwin'), .before=winprob) %>%
  rename(surface=racesurfacename) %>%
  filter(!racecode %in% c('GA', 'S', 'W', 'E', 'X', 'IA', 'P'), 
         conf_level >= 0.0
         #month %in% c( 'Jul', 'Aug'), # meetingdate >= '2026-06-11', #bf_prob_5 > bf_prob_60
         ) %>%
  mutate(bf_firm=ifelse(bf_prob_5 > bf_prob_60, 1, 0),
         bf_int = sim_ints[i],
         sim_n = j,
         seed_value=seed_val
         )
  

# check NA's in pnl file
test_pnl_dat_nas <- test_pnl_dat %>%
  select(raceid, all_of(model_vars)) %>%          # cull to raceid + model_vars
  select(raceid, where(~ any(is.na(.))))       # keep raceid + any col with an NA

# pnl tables
conf_levels = c(0, 0.04, 0.08)
for(j in 1:length(conf_levels)){
  mins = c(0, conf_levels[j], 0.1) # exp val; conf level; winprob min
  print(cbind(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), mins[1], mins[2], mins[3]), conf=conf_levels[j]))
}
print(pnl_table(test_pnl_dat, c( 'surface', 'month' ), 0, 0.0, 0.1)) # months
print(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index', 'bsp_bin' ), 0, 0.0, 0.1)) # bsp bins
print(pnl_table(test_pnl_dat, c( 'surface', 'winprob_rank' ), 0, 0, 0.1)) # winprob rank
print(pnl_table(test_pnl_dat, c( 'surface', 'raceclass' ), 0, 0, 0.1)) # winprob rank
print(pnl_table(test_pnl_dat, c( 'surface', 'racecode'), 0, 0, 0.1)) # winprob rank
print(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), 0, 0, 0.1)) # overall
print(pnl_table(test_pnl_dat, c( 'surface', 'bf_firm' ), 0, 0, 0.1)) # flucs

# list all test_pnl df's
sim_int_list[[length(sim_int_list)+1]] = test_pnl_dat
message("Run seed: ", seed_val) 
} # j sim loop
#} # market int i loop



# compare test pnl versions
test_pnl_comparison <- sim_int_list %>%
  bind_rows() %>%
  select(raceid, meetingdate, horsecode, horsename, numberofrunners, positionofficial, 
         fave_price, bf_price, winprice, profit, bf_int) %>%
  pivot_wider(
    id_cols = c(raceid, meetingdate, horsecode, horsename, numberofrunners, positionofficial),
    names_from = bf_int,
    values_from = c(fave_price, bf_price, winprice, profit),
    names_sep = "_"
  ) %>%
  #select(-ends_with('_0')) %>%
  filter(!is.na(winprice_120)&!is.na(winprice_60)&!is.na(winprice_10)&!is.na(winprice_5)) %>%
  mutate(across(starts_with('profit'), ~round(., 2)),
         across(starts_with('winprice'), ~round(., 2)))

# export results csv
# write.csv(test_pnl_dat, paste0('test_pnl_AW_',  Sys.Date(), '.csv') , row.names=F, na='')

# save rds file
# saveRDS(final_model, file = paste0('gb_AW_winprobs_', Sys.Date(), '.rds'))

# xgboost::xgb.plot.multi.trees(model = xgb_booster, feature_names = xgb_booster$feature_names)
