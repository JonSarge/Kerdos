#################################################
# KERDOS - GB Horse Racing Model
#
# updated: 18.06.2026
#
# Actions:
#  - data preprocessing
#  - feature engineering
#  - generates test winprobs
#################################################

# load libraries
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(tidymodels)


#### LOAD SOURCE/MODEL DATA ####

# load recent race data to match software winprobs to
gb_pre_race_data <- read.csv('performance_data_2026-06-23_to_2026-06-23_1782200208628.csv')

# load rds model object
final_model <- readRDS('gb_turf_winprobs_2026-06-23.rds')

##################################

# base race variables
race_vars <- c(
  'raceid', 
  'meetingdate', 
  'scheduledtimeofracegmt',
  'racesurfacename',
  'raceclass',
  'racecode',
  'numberofrunners',
  'coursename', 
  'racenumber',
  'horsename',
  'positionofficial'
  )

# factor vars
factor_vars = c(
  'racetype', 
  'raceclass', 
  'racesurfacename',  
  'racecode', 
  'going', 
  'iphintsoverallpace', 
  'pacemapdata', 
  'finish_group', 
  'field_size'
  )

# model features
model_vars = c(
  'kerdos_rating',
  'rating_rank',
  'bsp_prob_run1_fluc',
  'bsp_prob',
  'field_size',
  'draw_relative',
  'distance_m', 
  'weightcarried', 
  'horseage',
  'daysoff',
  'pace_pred'
)


# target var
target_var = 'finish_group'

#### ratings analysis ####

# run this code on the raceday data prior to generating winprobs
gb_race_test_data <- gb_pre_race_data %>%
  # add/edit relevant features
  mutate(meetingdate = ymd_hms(meetingdate),
         year=year(meetingdate),
         month=month(meetingdate, label = TRUE, abbr = TRUE),
         # finish group (target var)
         finish_group = case_when(
           positionofficial == 1 ~ "Win",
           positionofficial %in% 2:3 ~ "Place",
           positionofficial %in% 4:6 ~ "Unplaced", 
           TRUE ~ "Out_of_Money"
         ),
         finish_group = factor(finish_group, levels = c("Win", "Place", "Unplaced", "Out_of_Money")),
         # field size binned
         field_size = case_when(
           numberofrunners < 6 ~ 'Small',
           between(numberofrunners, 6, 10) ~ 'Medium',
           between(numberofrunners, 11, 16) ~ 'Large',
           TRUE ~ 'Very Large'
         ),
         # relative draw (to field size)
         draw_relative = (draw - 1) / (numberofrunners - 1),
         # fill raceclass NA's
         raceclass_pred = case_when(
           racecode %in% c('G1','G2','G3','L') ~ 1,
           ratinglimitupper > 105 ~ 1, # CHECK THIS!!!
           between(ratinglimitupper, 100, 105) ~ 2,
           between(ratinglimitupper, 86, 99) ~ 3,
           between(ratinglimitupper, 76, 85) ~ 4,
           between(ratinglimitupper, 66, 75) ~ 5,
           between(ratinglimitupper, 45, 65) ~ 6,
           TRUE ~ raceclass
         ),
         raceclass = ifelse(is.na(raceclass), raceclass_pred, raceclass),
         # convert distance to meters
         distance_m=distance*201.168,
         # days off NA to 0
         daysoff = ifelse(is.na(daysoff), 0, daysoff),
         # pace prediction
         pace_pred = as.numeric(str_extract(pacemapdata, '\\d+$')),
         # market implied prob
         bsp_prob=1/betfairwinsp,
         run1_bsp_prob=1/run1_betfairwinsp,
         # bsp run1 to run0 bsp fluc
         run1_betfairwinsp = ifelse(is.na(run1_betfairwinsp)|run1_betfairwinsp==0, betfairwinsp, run1_betfairwinsp),
         bsp_run1_fluc = (run1_betfairwinsp-betfairwinsp)/run1_betfairwinsp,
         bsp_run1_fluc = ifelse(bsp_run1_fluc < -2, -2, bsp_run1_fluc),
         bsp_prob_run1_fluc = (bsp_prob - run1_bsp_prob)/run1_bsp_prob,
         bsp_prob_run1_fluc = ifelse(is.na(bsp_prob_run1_fluc), 0, bsp_prob_run1_fluc)

  ) %>%
  # filter out non-runners (for post-race data matching only)
  filter(positionofficial > 0) %>%
  # race-specific vars
  group_by(raceid) %>%
  mutate(fave_price=min(ispdecimal),
         # stanardise kerdos rating
         kerdos_rating = as.numeric(scale(pre_race_rating_adjusted)),
         # add kerdos rating rank per race 
         rating_rank=dense_rank(-pre_race_rating_adjusted)) %>% 
  ungroup() %>%
  # factor vars
  mutate(across(all_of(factor_vars), ~as.factor(.))) %>%
  # select model features
  select(all_of(race_vars), target_var, all_of(model_vars)) %>%
  arrange(scheduledtimeofracegmt) %>%
  # final filters
  filter(racesurfacename == 'All Weather',
         raceclass %in% c(5:6))
         

# run test winprobs to match software winprobs to
winprobs <- predict(final_model, gb_race_test_data, type = "prob")

# test output file with winprobs 
gb_race_test_preds <- gb_race_test_data %>%
  # attach winprobs
  bind_cols(winprobs[1]) %>%
  rename(winprob=.pred_Win) %>%
  # sum(winprob)=1.0
  group_by(raceid) %>%
  mutate(winprob_adj=winprob/sum(winprob)) %>%
  ungroup()


