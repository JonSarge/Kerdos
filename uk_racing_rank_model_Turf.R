#################################################
# KERDOS V4
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


manualUpload = 0
loadNewData = 0

#### import data  preprocess ####
source('uk_racing_data.R')

# declare target var
modelType  = 'binary' # binary or 'multinomial' 
target_var = ifelse(modelType == 'binary', 'finish_group_binary', 'finish_group')

# Predict finish position using tidymodels
model_vars = c(
  #'bsp_prob_run1_fluc',
  'bf_prob_60_run1_fluc',
  'kerdos_rating',
  'kerdos_rating_rank',
  'fave_price',
  'category_rank',
  'raceclass',
  'category_blended_score',
  'draw_relative',
  'distance_m',
  'weightcarried', # 
  'horseage',
  'daysoff',
  #'horsegender',
  #'bf_prob_60'
  #'bf_prob_10',  
  #'bsp_prob_5',
  #'bf_prob_60_10'
  'bf_prob_60_10',
  'bf_prob_60',
  'bf_60_rank',
  #'bf_prob_10_5',
  #'bf_prob_60_5',
  # 'bsp_logit_60',
  # 'bsp_logit_10',
  # 'bsp_logit_5',
  # 'bsp_ratio_60_10',
  # 'bsp_ratio_60_5',
  # 'bsp_ratio_60_0',
  # 'bsp_pct_60_10'
  # 'bsp_rate_60_10',
  # 'bsp_rate_10_5',
  # 'bsp_accel_late',
  # 'bsp_accel_mid',
  # 'monotonic_steam',
  # 'monotonic_drift',
  # 'reversal'
  #'jockey_rating',
  #'trainer_rating',
  #'jockeyinform',
  #'hottrainer',
  #'toprated',
  #'ratingstars',
  #'field_size',
  #'bharating',
  #'rating0to100',
  #'rating123',
  #'tissuepricedecimal',
  #'significantimprover',
  #'goingabbrev',
  #'iphintsoverallpace',
  'pace_pred'
)

# tf rating na races
tf_rating_nas <- pre_model_data %>% 
  select(raceid, meetingdate, toprated, bharating, rating0to100, jockeyinform, pace_pred,
         hottrainer, ratingstars, rating123, tissuepricedecimal, significantimprover) %>% 
  filter(!complete.cases(.)) %>%
  pull(raceid) %>% unique()

logit <- function(p) log(p / (1 - p))

# modelling df w. final filters    
model_data <- pre_model_data %>%  
  filter(!raceid %in% ratings_remove$raceid, # CHECK THESE TO ADD BACK IN!!!
         !(preraceadjustedrating >= 70 & kerdos_rating < 30), # fix this in racing data script
         racesurfacename == 'Turf',
         raceclass %in% c(2:4),
         !racecode %in% c('S', 'W', 'GA', 'IM'),
         #!raceid %in% tf_rating_nas,
         #!raceid %in% jockey_nas$raceid,
         !is.na(pace_pred) # FIX THIS!!!
  ) %>% 
  group_by(raceid) %>% # add this to pre_model_dat
  mutate(jockey_rating = nm(scale(jockey_rating)),
         bf_60_rank=rank(-bsp_60, ties.method="min")/numberofrunners) %>%
  ungroup() %>%
  mutate(bf_prob_60 = 1/bsp_60,
         bf_prob_10 = 1/bsp_10,
         bf_prob_5 = 1/bsp_5,
         bf_prob_60_10 = bf_prob_10 - bf_prob_60,
         bf_prob_60_5 = bf_prob_5 - bf_prob_60,
         bf_prob_10_5 = bf_prob_5 - bf_prob_10,
         # bsp_prob_60_0 = bsp_prob - bsp_prob_60,
         # bsp_prob_10_0 = bsp_prob - bsp_prob_10,
         # bsp_prob_5_0 = bsp_prob - bsp_prob_5,
          bf_logit_60 = logit(bf_prob_60),
          bf_logit_10 = logit(bf_prob_10),
         # bsp_logit_5  = logit(bsp_prob_5),
         # bsp_logit_0  = logit(bsp_prob),
          bf_logit_60_10 = bf_logit_10 - bf_logit_60,
         # bsp_logit_60_5  = bsp_logit_5  - bsp_logit_60,
         # bsp_logit_60_0  = bsp_logit_0  - bsp_logit_60,
         # bsp_ratio_60_10 = bf_prob_10 / bf_prob_60,
         # bsp_ratio_60_5  = bsp_prob_5  / bsp_prob_60,
         # bsp_ratio_60_0  = bsp_prob   / bsp_prob_60,
         # bsp_pct_60_10   = (bsp_prob_10 - bsp_prob_60) / bsp_prob_60,
          bf_rate_60_10 = bf_prob_60_10 / 50,
         # bsp_rate_10_5  = bsp_prob_10_5 / 5,
         # bsp_rate_5_0   = bsp_prob_5_0 / 5,
         # bsp_accel_late = bsp_rate_5_0 - bsp_rate_10_5,
         # bsp_accel_mid  = bsp_rate_10_5 - bsp_rate_60_10,
         # monotonic_steam = as.integer(bsp_prob_60_10 > 0 & bsp_prob_10_5 > 0 & bsp_prob_5_0 > 0),
         # monotonic_drift = as.integer(bsp_prob_60_10 < 0 & bsp_prob_10_5 < 0 & bsp_prob_5_0 < 0),
         # reversal = as.integer(sign(bsp_prob_10_5) != sign(bsp_prob_5_0)),
         bf_prob_10_run1_fluc = (bf_prob_10 - run1_bsp_prob)/run1_bsp_prob,
         bf_prob_10_run1_fluc = ifelse(is.na(bf_prob_10_run1_fluc), 0, bf_prob_10_run1_fluc),
         bf_prob_60_run1_fluc = (bf_prob_60 - run1_bsp_prob)/run1_bsp_prob,
         bf_prob_60_run1_fluc = ifelse(is.na(bf_prob_60_run1_fluc), 0, bf_prob_60_run1_fluc)
         #bf_prob_60_damped=bsp_prob^0.1,
         ) %>%
  filter(!is.na(bf_prob_60))

# check ratings correlate
hist_data <- model_data %>% filter(!preraceadjustedrating %in% c(0,999))
plot(hist_data$preraceadjustedrating, hist_data$kerdos_rating,
     main='Kerdos-Timeform Relationship', xlab='Timeform', ylab="Kerdos")

# CHECK model data na's
model_data_nas <- model_data %>% 
  select(where(~ any(is.na(.)))) %>% 
  filter(if_any(everything(), is.na)) 

if(length(race_vars[race_vars %in% names(model_data_nas)])>0) "CHECK NAs IN RACE VARS!!!"
if(length(model_vars[model_vars %in% names(model_data_nas)])>0) "CHECK NAs IN MODEL VARS!!!"

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
         #between(betfairwinsp, 1.2, 50)
         ) %>%
  select(raceid, meetingdate, all_of(model_vars), all_of(target_var)) # final select

# final check for training data na's
train_data_nas <- train_data %>% filter(!complete.cases(.))

test_data <- model_data %>% 
  filter(meetingdate >= test_date_start,
         meetingdate <= test_date_end,
  ) 


#### tune xgboost model ####

# Create tuning grid with specified values
trees = 500 # specify n trees sequence
min_n = 5 # specify minimum tree n sequenc
tree_depth = 3
learn_rate = 0.1
loss_reduction = 0.01
source('uk_racing_rank_model_tune.R') # rank model

 
 