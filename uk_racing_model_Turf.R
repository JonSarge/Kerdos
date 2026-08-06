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
#  - daily price fllucs
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


manualUpload = 0
loadNewData = 0

#### import data  preprocess ####
source('uk_racing_data.R')

# declare target var
modelType  = 'binary' # binary or 'multinomial' 
target_var = ifelse(modelType == 'binary', 'finish_group_binary', 'finish_group')

# Predict finish position using tidymodels
model_vars = c(
  'bsp_prob_run1_fluc',
  'kerdos_rating',
  'kerdos_rating_rank',
  'fave_price',
  'bsp_prob',
  'bsp_rank',
  'category_rank',
  'category_blended_score',
  'draw_relative',
  'distance_m',
  'weightcarried', # weight_relative
  'horseage',
  'daysoff'
  #'jockey_rating',
  #'trainer_rating',
  #''raceclass', 
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
  #'pace_pred'
)

# tf rating na races
tf_rating_nas <- pre_model_data %>% 
  select(raceid, meetingdate, toprated, bharating, rating0to100, jockeyinform, pace_pred,
         hottrainer, ratingstars, rating123, tissuepricedecimal, significantimprover) %>% 
  filter(!complete.cases(.)) %>%
  pull(raceid) %>% unique()

# modelling df w. final filters    
model_data <- pre_model_data %>%  
  filter(!raceid %in% ratings_remove$raceid, # CHECK THESE TO ADD BACK IN!!!
         !(preraceadjustedrating >= 70 & kerdos_rating < 30), # fix this in racing data script
         racesurfacename == 'Turf',
         raceclass %in% c(2:6),
         !racecode %in% c('S', 'W', 'GA', 'IM'),
         #!raceid %in% tf_rating_nas,
         #!raceid %in% jockey_nas$raceid
         #!is.na(pace_pred) # FIX THIS!!!
  ) %>% 
  group_by(raceid) %>% # add this to pre_model_dat
  mutate(jockey_rating = nm(scale(jockey_rating))) %>%
  ungroup() 


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
         #between(betfairwinsp, 1.1, 50)
         ) %>%
  select(raceid, meetingdate, all_of(model_vars), all_of(target_var)) # final select

# final check for training data na's
train_data_nas <- train_data %>% filter(!complete.cases(.))


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
source('uk_racing_model_tune.R') # tuning script

#### generate results  PnL ####

test_pnl_dat <- pnl_df(c(1:5)) %>%
  select(all_of(race_vars), month, all_of(model_vars), c(win:conf_level), 
         Rating_Jockey, Rating_Trainer, -winclass) %>%
  relocate(starts_with('betfairwin'), .before=winprob) %>%
  rename(surface=racesurfacename) %>%
  filter(betfairwinsp <= 100)


# final check for training data na's
test_data_nas <- test_pnl_dat %>% 
  select(-c(ave_win_bsp, run1_raceclass, starts_with('Rating_'))) %>% 
  filter(!complete.cases(.))

# pnl tables
conf_levels = c(0, 0.025, 0.06)
for(i in 1:length(conf_levels)){
  mins = c(0, conf_levels[i], 0.1) # exp val; conf level; winprob min
  print(cbind(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), mins[1], mins[2], mins[3]), conf=conf_levels[i]))
}
pnl_table(test_pnl_dat, c( 'surface', 'month' ), 0, 0.0, 0.1) # months
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index', 'bsp_bin' ), 0, 0.0, 0.1) # bsp bins
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), 0, 0, 0.1) # overall
pnl_table(test_pnl_dat, c( 'surface', 'winprob_rank' ), 0, 0, 0.1) # winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'raceclass' ), 0, 0, 0.1) # winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'racecode' ), 0, 0, 0.1) # winprob rank



# check model v market
market_model_df <- test_pnl_dat %>% 
  filter(betfairwinsp < 20) %>%
  select(raceid, bsp_prob, winprob_adj, betfairwinsp, winprice)
plot(market_model_df$betfairwinsp, market_model_df$winprice)

# export results
write.csv(test_pnl_dat, paste0('test_pnl_Turf_',  Sys.Date(), '.csv') , row.names=F, na='')


#### jockey/trainer rating check ####

# estimation model for >50% na's in race

jockey_pnl_dat <- test_pnl_dat %>%
  group_by(raceid) %>%
  mutate(rating_nas = sum(is.na(Rating_Jockey))) %>%
  filter(rating_nas/numberofrunners < 0.5) %>% 
  mutate(jockey_rating=ifelse(is.na(Rating_Jockey), min(na.omit(Rating_Jockey)), Rating_Jockey)) %>%
  ungroup() %>%
  relocate(Rating_Jockey, .before=jockey_rating)

jockey_pnl_summ <- jockey_pnl_dat %>%
  mutate(jockey_rating=ifelse(is.na(Rating_Jockey), min(na.omit(Rating_Jockey)), Rating_Jockey)) %>%
  ungroup() %>%
  relocate(Rating_Jockey, .before=jockey_rating)

jockey_pnl_summ <- jockey_pnl_dat %>%
  group_by(raceid) %>%
  filter(jockey_rating==max(jockey_rating) & betfairwinsp < 30) %>% ungroup() %>%
  mutate(ave_win_bsp=ifelse(win==1, betfairwinsp, NA)) %>%
  #group_by(bsp_index, bsp_bin) %>%
  summarise(bets=n(),
            sr=mean(win),
            ave_win=mean(na.omit(ave_win_bsp)),
            profit=sum(profit),
            roi=profit/bets) %>% ungroup()


trainer_pnl_dat <- test_pnl_dat %>%
  group_by(raceid) %>%
  mutate(rating_nas = sum(is.na(Rating_Trainer))) %>%
  filter(rating_nas/numberofrunners < 0.5) %>% 
  mutate(trainer_rating=ifelse(is.na(Rating_Trainer), min(na.omit(Rating_Trainer)), Rating_Trainer)) %>%
  ungroup() %>% relocate(Rating_Trainer, .before=trainer_rating)

trainer_pnl_summ <- trainer_pnl_dat %>%
  group_by(raceid) %>%
  filter(trainer_rating==max(trainer_rating) & betfairwinsp <= 30) %>% ungroup() %>%
  mutate(ave_win_bsp=ifelse(win==1, betfairwinsp, NA)) %>%
  #group_by(bsp_index, bsp_bin) %>%
  summarise(bets=n(),
            sr=mean(win),
            ave_win=mean(na.omit(ave_win_bsp)),
            profit=sum(profit),
            roi=profit/bets)



#### final checks ####

test_pnl_filtered <- test_pnl_dat %>%
  mutate(pick = ifelse(exp_value >= 0 &
                       conf_level >= 0.0 &
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

 
 