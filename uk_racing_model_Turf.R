#################################################
# KERDOS
# UK Horse Racing Model
#
# commenced: 03.06.2025; updated: 21.05.2026
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
#
# Ask Mike:
#  - what markets (place betting?)
#  - opening fixed prices?
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

ch=function(x){as.character(x)}
fct=function(x){as.factor(x)}
nm=function(x){as.numeric(x)}
hms_times=function(x){format(as.POSIXct(x, origin="1970-01-01",tz="UTC"), '%H:%M')}
options(dplyr.summarise.inform = FALSE)
# gerenerate test pnl df 
pnl_df <- function(ranks){
  dat <- bind_cols(test_data, 
                   winprob=predictions$.pred_Win,
                   winclass=predictions$.pred_class) %>%
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
           ave_win_bsp=ifelse(win==1, betfairwinsp, NA),
           profit=win*betfairwinsp-1,
           bf_prob = 1/(betfairwinsp),
           exp_value = (betfairwinsp - winprice)/winprice,
           conf_level = winprob - bf_prob
    ) %>% rename(surface=racesurfacename) 
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
  return(sum(x$profit)/sum(x$bets))
}
chart.rpart<-function(model, title){rpart.plot(model, digits=3, type=3, extra = 101, main=title)}

# Set up parallel processing
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

manualUpload = 0
loadNewData = 1

if(manualUpload==1){
  # manually load data
  load('gb_pre_model_data.rda')
  load('rating_anomalies.rda')
} else {
  # generate from raw csv data
  source('uk_racing_data.R')
}

# target var
target_var = 'finish_group'

# Predict finish position using tidymodels
model_vars = c(
  'kerdos_rating',
  'rating_rank',
  'bsp_prob_run1_fluc',
  'bsp_prob',
  'field_size',
  'raceclass', 
  'draw_relative',
  'distance_m', 
  'weightcarried', # weight_relative
  'horseage',
  'daysoff',
  'pace_pred'
  #'prizefund'
  #'goingabbrev'
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
  filter(!raceid %in% rating_anomalies$raceid, # CHECK THESE TO ADD BACK IN!!!
         racesurfacename == 'Turf',
         raceclass %in% c(5:6),
         !is.na(pace_pred) # FIX THIS!!!
  ) 

# CHECK model data na's
model_data_nas <- model_data %>% select(where(~ any(is.na(.)))) %>% filter(if_any(everything(), is.na))
  

# model formula
model_formula = as.formula(paste(target_var, '~', paste(model_vars, collapse = " + ")))
# train/test dates
train_date_start = '2024-01-01'
test_date_start = '2026-01-01'   # run from 1st June with training data up to 30th May
test_date_end = '2026-12-31'

# training sample w final filtering
train_data <- model_data %>% 
  filter(meetingdate >= train_date_start,
         meetingdate < test_date_start
  ) %>%
  select(raceid, meetingdate, all_of(model_vars), all_of(target_var)) # final select

# check training data na's
train_data_nas <- train_data %>% filter(if_any(everything(), is.na))


#### tune xScore model ####
TreeModel = c('lightgbm', 'xgboost', 'glmnet')
t = 2 # 1=gbm; 2=xgb; 3=glmnet
# tuning
tuneModels = 0
randomGrid = 0
randomGridSize = 10 
PenMix = 0 # glmnet Penalty mixture: pure Ridge; 1=pure Lasso
PenValue = 0.01
# Create tuning grid with specified values
trees = 500 # specify n trees sequence
min_n = 10 # specify minimum tree n sequenc
tree_depth = 3
learn_rate = 0.1
loss_reduction = 0.01
source('uk_racing_model_tune.R') # tuning script

# get test pnl df (with winprob ranks)
test_pnl_dat <- pnl_df(c(1:4))
# pnl tables
conf_levels = c(0, 0.04, 0.1)
for(i in 1:length(conf_levels)){
  mins = c(0, conf_levels[i], 0.1) # exp val; conf level; winprob min
  print(cbind(pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), mins[1], mins[2], mins[3]), conf=conf_levels[i]))
}
# months
pnl_table(test_pnl_dat, c( 'surface', 'month' ), 0, 0.0, 0.1) 
# bsp bins
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index', 'bsp_bin' ), 0, 0.0, 0.1)
# overall
pnl_table(test_pnl_dat, c( 'surface', 'bsp_index_all' ), 0, 0, 0.1)
# winprob rank
pnl_table(test_pnl_dat, c( 'surface', 'winprob_rank' ), 0, 0, 0.1)


# export results
write.csv(test_pnl_dat, paste0('test_pnl_Turf_',  Sys.Date(), '.csv') , row.names=F, na='')

# save rds file
saveRDS(final_model, file = 'gb_turf_winprobs_2026-06-24.rds')





# send script to repo
git_add("uk_racing_model.R")
git_commit("Update data script")
git_push()






# check dead-heats
dead_heats <- test_pnl_dat %>% 
  count(raceid, positionofficial) %>% 
  filter(positionofficial==1 & n > 1)
          
test_pnl_filtered <- test_pnl_dat %>%
  mutate(pick = ifelse(exp_value >= 0 &
                       conf_level >= 0.04 &
                       winprob_adj >= 0.1, 
                1, 0))

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
         
# save current model before trialling new one
save(final_model, file=paste0('gb_racing_model_', Sys.Date(), '.rds'))
  
# check winprob hist
hist(test_pnl_dat$winprob_adj)
  
# export results
write.csv(test_pnl_dat, paste0('test_pnl_aw_', Sys.Date(), '.csv'), row.names = F, na='')



#### rpart bet strategy ####
rpart_dat <- test_pnl_dat %>% 
  filter(pick==1,
         pos_value == 1,
         #exp_value >= 0.04,
         bsp_index==3
         )

racing_roi <- rpart(profit ~ exp_value, rpart_dat, minbucket=30)
chart.rpart(racing_roi, paste0('GB Racing ROI (n=', nrow(rpart_dat), ')'))




 
 
 #### save xgb model for python ####

 saveRDS(final_model, file='gb_racing_winprobs_2026-03-02.rds')
 
 # Extract the XGBoost engine from the workflow
 xgb_model <- final_model %>% extract_fit_engine()
 # Now save it
 xgb.save(xgb_model, "gb_racing_winprobs_2026-03-02.xgb")
 
 # Get the preprocessed training data (before model fitting)
 preprocessed_train <- recipe_ml %>%
   prep(training = train_data) %>%
   bake(new_data = NULL) %>%
   head(105)
 
 write.csv(preprocessed_train, 'training_dat_pre_norm_2026-03-02.csv', row.names = F)
 
 
 # winprob dist's
 winprob_dist <- test_pnl_dat %>% 
   group_by(raceid) %>%
   summarise(n(), sum(winprob), sum(winprob_adj))
 
 # bets-per-race
 bets_per_race <- test_pnl_dat %>% group_by(raceid) %>% summarise(sum(winprob))
 
 
 