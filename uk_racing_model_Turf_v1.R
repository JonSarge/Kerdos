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

# Set up parallel processing
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

loadNewData = 0

#### import data  preprocess ####
source('uk_racing_data.R')

# declare target var
target_var = 'finish_group_binary' # 'binary', 'finish_group_binary', 'finish_group')

# Predict finish position using tidymodels
model_vars = c(
  'kerdos_rating_adj',
  'kerdos_rating_rank',
  'fave_price',
  'category_blended_score',
  'draw_relative',
  'distance_m',
  'weightcarried',
  'horseage',
  'daysoff',
  'sb_m',
  'bf_prob_60',
  'raceclass',
  'pace_pred'
  )


# modelling df w. final filters    
model_data <- pre_model_data %>%  
  filter(!raceid %in% ratings_remove$raceid, # CHECK THESE TO ADD BACK IN!!!
         !(preraceadjustedrating >= 70 & kerdos_rating < 30), # fix this in racing data script
         racesurfacename == 'Turf',
         raceclass %in% c(2:6),
         !racecode %in% c('S', 'W', 'GA', 'IM'),
         !is.na(pace_pred), # FIX THIS!!!
         !is.na(bf_prob_60), # FIX THIS!!!
  ) %>% 
  mutate(bf_prob_60 = 1/bsp_60,
         bf_prob_5 = 1/bsp_5)

# model formula
model_formula = as.formula(paste(target_var, '~', paste(model_vars, collapse = " + ")))

# train/test dates
train_date_start = '2024-01-01'
test_date_start = '2026-01-01'   # run from 1st June with training data up to 30th May
test_date_end = '2026-12-31'

# training sample w final filtering
train_data <- model_data %>% 
  filter(meetingdate >= train_date_start,
         meetingdate < test_date_start,
         between(betfairwinsp, 1.1, 99)
         ) %>%
  select(raceid, meetingdate, all_of(model_vars), all_of(target_var)) # final select

# final check for training data na's
train_data_nas <- train_data %>% filter(!complete.cases(.))

# test data
test_data <- model_data %>% filter(meetingdate >= test_date_start & meetingdate <= test_date_end)


#### tune xgboost model ####

# tuning
tuneModels = 1
randomGrid = 1
randomGridSize = 10 

# Create tuning grid with specified values
trees = 500 # specify n trees sequence
min_n = 10 # specify minimum tree n sequenc
tree_depth = 3
learn_rate = 0.1
loss_reduction = 0.01
setSeed=0
source('uk_racing_model_tune.R') # xbg tuning script


#### generate results  PnL ####

test_pnl_dat <- pnl_df(c(1:4)) %>%
  select(all_of(race_vars), month, all_of(model_vars), c(win:conf_level), 
         Rating_Jockey, Rating_Trainer, -winclass, goingabbrev) %>%
  relocate(starts_with('betfairwin'), .before=winprob) %>%
  rename(surface=racesurfacename) %>%
  filter(betfairwinsp <= 100, # bf_prob_5 > bf_prob_60,
         ) %>%
  mutate(bf_firm=ifelse(bf_prob_5 > bf_prob_60, 1, 0))

# pnl tables
conf_levels = c(0, 0.025, 0.075)
for(i in 1:length(conf_levels)){
  mins = c(0, conf_levels[i], 0.1) # exp val; conf level; winprob min
  print(cbind(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), mins[1], mins[2], mins[3]), conf=conf_levels[i]))
}
pnl_table(test_pnl_dat, c( 'surface', 'month' ), 0, 0.0, 0.1) # months
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index', 'bsp_bin' ), 0, 0.0, 0.1) # bsp bins
pnl_table(test_pnl_dat, c( 'surface', 'winprob_rank' ), 0, 0, 0.1) # winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'raceclass' ), 0, 0, 0.1) # winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'racecode' ), 0, 0, 0.1) # winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), 0, 0, 0.1) # overall
pnl_table(test_pnl_dat, c( 'surface', 'bf_firm' ), 0, 0, 0.1) # overall


