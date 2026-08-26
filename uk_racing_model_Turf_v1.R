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
  "dplyr", "stringr", "tidymodels", "tidyverse", "vip",
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

loadNewData = 0

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
  'sb_m',
  #'bf_prob_60',
  'bf_prob_60',
 #'bsp_rank',
  'bf_prob_10_rank',
  'raceclass',
  'racecode',
  'pace_pred',
  'jockey_rating_top',
  'trainer_rating_top'
  #'bf_prob_60_run1_fluc'
  #'bf_prob_60_5'
  #'field_size',
  #'bf_under_5'
  # 'runners_under_5'
  )

# try a top jockey/trainer dummy var
# call bf_prob_60, bf_prob_60_5  

# modelling df w. final filters    
model_data <- pre_model_data %>%  
  filter(!raceid %in% ratings_remove$raceid, # CHECK THESE TO ADD BACK IN!!!
         !(preraceadjustedrating >= 70 & kerdos_rating < 30), # fix this in racing data script
         racesurfacename == 'Turf',
         raceclass %in% c(5:6), # Separate model for C1 races
         !racecode %in% c('S', 'W', 'GA', 'IM'), # low quality/sample races
         !is.na(pace_pred), # FIX THIS!!!
         !is.na(bf_prob_60),
         #between(betfairwinsp, 1.1, 50),
  ) %>%
  mutate(jockey_rating_bin=round(jockey_rating/10),
         jockey_rating_bin=ifelse(is.na(jockey_rating_bin), 5, jockey_rating_bin),
         jockey_rating_top=case_when(
           jockey_rating_bin %in% c(9:10) ~ 1,
           jockey_rating_bin %in% c(5:8) ~ 2,
           jockey_rating_bin %in% c(1:4) ~ 3,
           TRUE ~ 3),
         trainer_rating_bin=round(Rating_Trainer/10),
         trainer_rating_bin=ifelse(is.na(trainer_rating_bin), 5, trainer_rating_bin),
         trainer_rating_top=case_when(
           trainer_rating_bin %in% c(8:10) ~ 1,
           trainer_rating_bin %in% c(5:7) ~ 2,
           trainer_rating_bin %in% c(1:4) ~ 3,
           TRUE ~ 3)
         ) %>%
  mutate(across(c(jockey_rating_top, trainer_rating_top), ~as.factor(.)))

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
         #kerdos_rating_rank %in% c(1:6)
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
setSeed=0
source('uk_racing_model_tune.R') # xbg tuning script

#### generate results ####
test_pnl_dat <- pnl_df(c(1:4)) %>%
  select(all_of(race_vars), month, all_of(model_vars), c(win:conf_level), 
         Rating_Jockey, Rating_Trainer, -winclass, goingabbrev) %>%
  relocate(starts_with('betfairwin'), .before=winprob) %>%
  rename(surface=racesurfacename) %>%
  filter(betfairwinsp <= 100,
         #raceclass %in% c(5,6),
         #bf_prob_5 > bf_prob_60,
         ) %>%
  mutate(bf_firm=ifelse(bf_prob_5 > bf_prob_60, 1, 0))

# pnl tables
conf_levels = c(0, 0.04, 0.08)
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
pnl_table(test_pnl_dat, c( 'surface', 'bf_firm' ), 0, 0, 0.1) # flucs


# export results csv
# write.csv(test_pnl_dat, paste0('test_pnl_Turf_',  Sys.Date(), '.csv') , row.names=F, na='')

# save rds file
# saveRDS(final_model, file = paste0('gb_turf_winprobs_', Sys.Date(), '.rds'))
