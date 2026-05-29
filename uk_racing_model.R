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

cdir = 'C:/Users/jonsa/OneDrive/STATS_ON/Projects/Mike_Spence'
setwd(cdir)

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
# pnl summary table
pnl_table <- function(dat, vars, val, conf, prob){
  x <- dat %>%
    filter(pick==1,
           #kelly > k,
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

# SQL connection
if ("package:RMySQL" %in% search()) {
  detach("package:RMySQL", unload = TRUE)
}
con <- dbConnect(
  MariaDB(),
  user     = "avnadmin",
  password = "",
  dbname   = "master-db",
  host     = "mysql-20d0ecf2-jonsargent15-c8e7.c.aivencloud.com",
  port     = 24081
)


# update data
loadNewData = 1
source('uk_racing_data.R')

# LOAD MODEL DATA
model_data <- dbGetQuery(con, "select distinct * from gb_racing_data where meetingdate >= '2024-01-01' ") # attach jockey/trainer codes to this

numeric_vars = c('racenumber', 'draw', 'raceclass', 'distance', 'distance_m', 'distancecumulative', 'distancebeaten', 'ispdecimal', 
                 'betfairwinsp', 'betfairplacesp', 'bsp_run1_fluc', 'fave_price', 'weightcarried', 'daysoff', 'lastrunflat', 'horseage', 'eligibilityagemin', 'bharating', 
                 'wfa', 'apprenticeclaim', 'weightcarried', 'weight_adjustment', 'ratinglimitlower', 'ratinglimitupper', 'preraceepf', 'pre_race_rating_raw', 
                 'postrace_rating_raw', 'pre_race_rating_adjusted', 'pre_race_rating_adjusted_y', 'preraceadjustedrating', 'preraceadjustedrating_y', 'preracemasterrating', 
                 'performancerating', 'ratingsadjustment', 'adj_figure', 'timefigure', 'bharating', 'tissuepricedecimal', 'pacemapdata', 'rating0to100', 'bsp_log', 'bsp_prob', 'numberofrunners', # 'weight_relative', 
                 'winsatcourse', 'winsatdistance', 'winsatcoursedistance', 'prizefund', 'profit', 'pre_race_rating_adjusted_z', 'pre_race_rating_adjusted_z_adj',
                 'preraceadjustedrating_z', 'preraceadjustedrating_z_adj', 'prior_ratings_adjusted', 'prior_timefigure') # 'jockey_win_place_ave', 'trainer_win_place_ave', 

# target var
target_var = 'finish_group'

model_data <- pre_model_data %>%
  # final select
  select(!na_cols,
         !zero_cols,
         all_of(race_vars), 
         all_of(numeric_vars), 
         all_of(factor_vars), 
         ends_with('trainercode'), 
         target_var)

# check any k-rating na's
model_data_krating_nas <- model_data %>% filter(is.na(pre_race_rating_adjusted))

# raceclass na's
raceclass_nas <- model_data %>% filter(is.na(raceclass))

# modelling df w/ NA's - CHECK FILTERS!!!         
model_data_w_nas <- model_data %>%  
  # estimate the NA K-Ratings here (prior to final rating_rank)
  mutate(pre_race_rating_adjusted=ifelse(is.na(pre_race_rating_adjusted),
                                         pre_race_rating_adjusted_y, pre_race_rating_adjusted)) %>%
  # add k-rating rank per race here (after any filtering) 
  group_by(raceid) %>%
  mutate(rating_rank=dense_rank(-pre_race_rating_adjusted),
         #rating_rank_y=dense_rank(-pre_race_rating_adjusted_y)
  ) %>% ungroup() %>%
  mutate(daysoff = ifelse(is.na(daysoff), 0, daysoff)) %>%
  select(target_var, raceid, meetingdate, year, horsename, racecode, numberofrunners,
         model_vars, betfairwinsp, bsp_index, bsp_bin, 
         positionofficial, preraceadjustedrating, raceclass, racecode, racesurfacename, class_prior) # , jockey_experience


# try rating_rank as factor

# final modelling data w/o NA's
model_data_final <- model_data_w_nas  %>% 
   filter(!raceid %in% rating_anomalies$raceid, # this accounts for outliers
          raceclass %in% c(4:6), # raceclass 1:4 with bsp_prob incl as feature = profitable $2.5-5!!!
          numberofrunners >= 6,
          racesurfacename == 'All Weather'
   ) %>%
   filter(complete.cases(.)) # some missing raceclasses and pacemapdata


###########################
# Set up parallel processing
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# target var
target_var = 'finish_group'

# Predict finish position using tidymodels
model_vars = c(#### numeric vars
  #'numberofrunners',
  #'raceclass', 
  #'racecode',
  'draw',
  'distance_m', 
  'weightcarried', 
  'horseage',
  'daysoff',
  'pre_race_rating_adjusted',
  #'pre_race_rating_adjusted_z_adj',
  'rating_rank',
  'bsp_run1_fluc',
  'bsp_prob',
  #'goingofficial_adj', # UN-BRACKETED FACTORS 
  'toprated',
  'pace_pred',
  'jockeyinform', # jockey ability more pronounced for all-weather (winter) 
  'class_prior',
  #'hottrainer',
  #'tissuepricedecimal'
  #'bharating', 
  #'rating0to100'
  #'ratingstars', # SIGNIFICANT
  #'rating123',
  'significantimprover'
)


# model formula
model_formula = as.formula(paste(target_var, '~', paste(model_vars, collapse = " + ")))
# train/test dates
train_date_start = '2024-01-01'
test_date_start = '2026-01-01' # run from 1st June with training data up to 30th May
test_date_end = '2026-12-31'


# training sample
train_data <- model_data_final %>% 
  filter(meetingdate >= train_date_start,
         meetingdate < test_date_start,
         #rating_rank %in% c(1:6)
  ) %>% 
  select(target_var, model_vars) 


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
source('uk_racing_model_tune_v3.R') # tuning script

# gerenerate test pl df 
test_pnl_dat <- bind_cols(test_data, 
                          winprob=predictions$.pred_Win,
                          winclass=predictions$.pred_class) %>%
   group_by(raceid) %>%
   mutate(
     winprob_adj=winprob/sum(winprob),
     winprob_rank=dense_rank(-winprob_adj),
     field_size = n()
   ) %>% ungroup() %>%
   mutate(month=month(meetingdate, label = TRUE, abbr = TRUE),
          bsp_bin=gsub('0-2.5', '1.01-2.5', bsp_bin),
          bsp_index_all=1,
          pick=ifelse(winprob_rank %in% c(1:6), 1, 0),
          winprice=1/winprob_adj, # model price
          win=ifelse(positionofficial==1, 1, 0),
          ave_win_bsp=ifelse(win==1, betfairwinsp, NA),
          profit=win*betfairwinsp-1,
          bf_prob = 1/(betfairwinsp),
          exp_value = (betfairwinsp - winprice)/winprice,
          conf_level = winprob - bf_prob
   ) %>% 
  filter(racecode == 'I', 
         #meetingdate > '2026-04-20'
         )
# pnl tables
pnl_table(test_pnl_dat, c( 'month' ), 0, 0.04, 0.1) 
pnl_table(test_pnl_dat, c( 'bsp_index', 'bsp_bin' ), 0, 0.04, 0.1)
pnl_table(test_pnl_dat, c( 'bsp_index_all' ), 0, 0.04, 0.1)

          
test_pnl_filtered <- test_pnl_dat %>%
  mutate(pick = ifelse(exp_value >= 0 &
                       conf_level >= 0.04 &
                       winprob_adj >= 0, 1, 0))

test_pnl_filtered %>% 
  group_by(pick) %>% 
  summarise(bets=n(), roi=mean(profit)) %>% ungroup() %>%
  mutate(bet_perc=bets/sum(bets))

bets_per_race = test_pnl_filtered %>%
  group_by(raceid) %>%
  summarise(bets=sum(pick),
            field=mean(numberofrunners))

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
 
 
 # interaction vars
 #step_interact(terms = ~ bsp_run1_fluc:pre_race_rating_adjusted) 
 # Handle missing values
 #step_impute_median(all_numeric_predictors()) %>%
 #step_impute_mode(all_nominal_predictors()) %>%
 # Handle class imbalance if needed
 #step_smote(finish_group, over_ratio = 0.5) %>%
 #  distance_m:pre_race_rating_adjusted) # var1:var2 + var1:var3 + var2:var3

 # CHECK: missing K/TF ratings
 # tf_missing_df <- gb_flat_all_cols %>% 
 #   filter(!between(preraceadjustedrating, 1, 998) | !between(pre_race_rating_adjusted, 1, 998)) %>%  
 #   select(race_vars, pre_race_rating_adjusted, preraceadjustedrating, bsp=betfairwinsp)
 # 
 # # CHECK: missing run1 ratings
 # na_run1_ratings <- gb_flat_all_cols %>%
 #   group_by(raceid) %>%
 #   filter(all(is.na(run1_post_race_rating_adjusted))) %>% ungroup() %>%
 #   select(race_vars, preraceadjustedrating, pre_race_rating_adjusted, ends_with('_post_race_rating_adjusted'))
 # 
 # k_missing_df <- gb_flat_all_cols %>%
 #   filter(#raceid %in% k_missing_raceids,
 #     !raceid %in% rating_anomalies$raceid
 #   ) %>%
 #   select(race_vars, pre_race_rating_adjusted, preraceadjustedrating) %>%
 #   mutate(kerdos_predict=predict(k_na_model, .)) %>%
 #   filter( raceid %in% c('1.47.1180130.4', '1.62.1200821.1', '1.1.1170715.2', '1.30.1181219.5', '1.42.1240520.5'))
 # 
