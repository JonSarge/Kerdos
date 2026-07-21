#################################################
# KERDOS V4
# UK Horse Racing Model
#
# commenced: 08.07.2026; updated: 08.07.2026
#
# Actions:
#  - data processing
#  - feature engineering
#  - multinomial logistic regression & xgboost models
#  - model accuracy comparison
#  - PnL report (flat-stake) 
#  - bet strategy
#
# To Do:
#  - jockey/trainer ratings
#  - daily price fllucs
#  - Binary win-loss model
#
# Ask Mike:
#  - 
#  - 
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

# tuning
tuneModels = 0
randomGrid = 0
randomGridSize = 10 
PenMix = 0 # glmnet Penalty mixture: pure Ridge; 1=pure Lasso
PenValue = 0.01

manualUpload = 0
loadNewData = 0

#### import data  preprocess ####
source('uk_racing_data.R')


modelType  = 'binary' # binary or 'multinomial' 
target_var = ifelse(modelType == 'binary', 'finish_group_binary', 'finish_group')

# Predict finish position using tidymodels
model_vars = c(
  #'kerdos_rating',
  'kerdos_rating_rank',
  #'bsp_prob_run1_fluc',
  #'fave_price',
  'bsp_prob',
  #'bsp_rank',
  #'field_size',
  #'racecode', 
  'draw_relative',
  'distance_m',
  'weightcarried', # weight_relative
  #'raceclass', 
  'horseage',
  'daysoff'
  #'pace_pred'
  #'Rating_Jockey'
  #'Rating_Trainer'
  #'prizefund'
  #'goingabbrev',
  #'iphintsoverallpace'
  #'class_prior'
  #'toprated',
  #'jockeyinform' # jockey ability more pronounced for all-weather (winter) 
  #'significantimprover',
  #'hottrainer',
  #'tissuepricedecimal'
  #'bharating', 
  #'rating0to100'
  #'ratingstars', # SIGNIFICANT
  #'rating123',
)

# modelling df w. final filters    
model_data <- pre_model_data %>%  
  filter(!raceid %in% ratings_remove$raceid, # CHECK THESE TO ADD BACK IN!!!
         racesurfacename == 'Turf',
         raceclass %in% c(2:4),
         racecode %in% c('I', 'IA'),
         #!is.na(pace_pred) # FIX THIS!!!
  ) # %>% mutate(kerdos_rating_rank = as.factor(kerdos_rating_rank))

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
         between(betfairwinsp, 1.3, 50)
         ) %>%
  select(raceid, meetingdate, all_of(model_vars), all_of(target_var)) # final select

train_data %>% filter(if_any(everything(), is.na))

# check training data na's
train_data_nas <- train_data %>% filter(if_any(everything(), is.na))


#### tune xgboost model ####

# Create tuning grid with specified values
trees = 500 # specify n trees sequence
min_n = 10 # specify minimum tree n sequenc
tree_depth = 3
learn_rate = 0.1
loss_reduction = 0.01
source('uk_racing_model_tune.R') # tuning script


#### generate results  PnL ####

test_pnl_dat <- pnl_df(c(1)) %>%
  select(all_of(race_vars), month, all_of(model_vars), c(win:conf_level), -winclass) %>%
  relocate(starts_with('betfairwin'), .before=winprob) %>%
  rename(surface=racesurfacename)
# pnl tables
conf_levels = c(0, 0.04, 0.1)
for(i in 1:length(conf_levels)){
  mins = c(0, conf_levels[i], 0.1) # exp val; conf level; winprob min
  print(cbind(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), mins[1], mins[2], mins[3]), conf=conf_levels[i]))
}
pnl_table(test_pnl_dat, c( 'surface', 'month' ), 0, 0.0, 0.1) # months
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index', 'bsp_bin' ), 0, 0.0, 0.1) # bsp bins
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), 0, 0, 0.1) # overall
pnl_table(test_pnl_dat, c( 'surface', 'winprob_rank' ), 0, 0, 0.1) # winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'raceclass' ), 0, 0, 0.1) # winprob rank


# export results
write.csv(test_pnl_dat, paste0('test_pnl_Turf_',  Sys.Date(), '.csv') , row.names=F, na='')



#### final checks ####

test_pnl_filtered <- test_pnl_dat %>%
  mutate(pick = ifelse(exp_value >= 0 &
                       conf_level >= 0.04 &
                       winprob_adj >= 0.1, 1, 0))

test_pnl_filtered %>% 
  group_by(pick) %>% 
  summarise(bets=n(), roi=mean(profit)) %>% ungroup() %>%
  mutate(bet_perc=bets/sum(bets))

bets_per_race = test_pnl_filtered %>%
  group_by(raceid) %>%
  summarise(bets=sum(pick),
            field=mean(numberofrunners),
            roi=sum(profit))

bets_per_race %>% group_by(bets) %>% summarise(freq=n()) %>% mutate(freq_perc=freq/sum(freq))
         
# check winprob hist
hist(test_pnl_dat$winprob_adj)
  


#### save rds file ####

saveRDS(final_model, file = 'gb_turf_winprobs_2026-06-24.rds')

 
 