#################################################
# KERDOS - v4 Flat Turf Racing Model
#
# updated: 11.09.2026
#
# Actions:
#  - data preprocessing
#  - feature engineering
#  - generates test winprobs
#################################################

library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(tidymodels)

ch=function(x){as.character(x)}
fct=function(x){as.factor(x)}
nm=function(x){as.numeric(x)}
options(dplyr.summarise.inform = FALSE); options(scipen=99)


#### LOAD MODEL & SOURCE DATA from wd ####

# load rds model object
final_model <- readRDS('gb_turf_winprobs_2026-09-11.rds')

# assume live source data is in this format
gb_data_recent <- read.csv("gb_recent_data.csv")

# jockey/trainer ratings data
jockey_ratings_df <- read.csv('jockey_ratings.csv')
trainer_ratings_df <- read.csv('trainer_ratings.csv')

# load flucs data
load("kerdos_flucs_data.rda")

# class ranks
class_rankings_old <- read.csv('gb_racing_raceclass.csv')
# class rank group summary
class_ranking_groups <- read.csv("class_ranks.csv", 
                                 comment.char="#", 
                                 fileEncoding = "latin1"
                                 )

#### FEATURE ENGINEERING #####

#### Jockey/Trainer ratings prep ####
jockey_ratings <- jockey_ratings_df %>% filter(Period==max(Period)) # get most recent
trainer_ratings <- trainer_ratings_df %>% filter(Period==max(Period))

# use mins to fill na's not in a jockey period
jockey_rating_mins <- jockey_ratings_df %>% 
  group_by(jockeyfullname=Jockey) %>% 
  summarise(Rating_Jockey_min=min(RATING)) %>% 
  ungroup()


#### class rank data prep ####
class_ranking_groups_fill <- class_ranking_groups %>%
  group_by(race_type, raceclass, age_category) %>%
  mutate(blended_score_ave=mean(blended_score),
         rank_median=median(rank),
         raceclass=as.factor(raceclass)) %>%
  ungroup() %>%
  select(racecode, raceclass, age_category, rank_median, blended_score_ave) %>%
  unique()

race_vars <- c('raceid', 'racenumber', 'meetingdate', 'scheduledtimeofracegmt',
               'coursename', 'horsecode', 'horsename', 'positionofficial', 
               'numberofrunners', 'betfairwinsp', 'raceclass', 'racecode', 
               'racesurfacename', 'saddlecloth', 'bsp_5')

# factor vars
factor_vars = c('racenumber', 'draw', 'positionofficial', 'horsegender', 'distancetype', 'racetype', 'raceclass',  
                'racesurfacename',  'racecode', 'going',  'iphintsoverallpace', 'pacemapdata', 'field_size',
                'jockeycode', 'jockeyfullname', 'trainercode', 'trainerfullname', 'ratingstars', 'rating123', 'significantimprover', 
                'jockeyinform', 
                'toprated', 'hottrainer', 'eligibilitysexlimit', 'preracemastersymbol', 'hotrace', 'pre_race_rating_symbol_for_16',
                'jockey_rating_top', 'trainer_rating_top')  


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
  'bf_prob_60',
  'bf_prob_60_rank',
  'raceclass',
  'pace_pred',
  'jockey_rating_top',
  'trainer_rating_top',
  'bf_prob_run1_fluc'
)


# run this code on the raceday data prior to generating winprobs
gb_race_test_data <- gb_data_recent %>%
  # primary model filters
  filter(countrycode=='GBR',
         racetype=='Flat',
         raceclass %in% c(2:6),
         positionofficial > 0
  ) %>%
  # add relevant vars (win, profit etc)
  mutate(meetingdate=as.Date(scheduledtimeofracegmt),
         year=year(meetingdate),
         month=month(meetingdate, label = TRUE, abbr = TRUE),
         # target variable (multinomial)
         finish_group = case_when(
           positionofficial == 1 ~ "Win",
           positionofficial %in% 2:3 ~ "Place",
           positionofficial %in% 4:6 ~ "Unplaced", 
           TRUE ~ "Out_of_Money"
         ),
         finish_group = factor(finish_group, levels = c("Win", "Place", "Unplaced", "Out_of_Money")),
         # target variable (binary)
         finish_group_binary = factor(
           ifelse(positionofficial == 1, "Win", "Not_Win"),
           levels = c("Win", "Not_Win")
         ),
         field_size = case_when(
           numberofrunners < 6 ~ 'Small',
           between(numberofrunners, 6, 10) ~ 'Medium',
           between(numberofrunners, 11, 16) ~ 'Large',
           TRUE ~ 'Very Large'
         ),
         # age category for class join
         age_category=ifelse(horseage <= 2, '2yo only', 'Older (3yo+, ages combined)'),
         # relative draw (to field sixze)
         draw_relative = (draw - 1) / (numberofrunners - 1),
         # convert distance to meters
         distance_m=distance*201.168,
         #daysoff adj
         daysoff = ifelse(is.na(daysoff), 0, daysoff),
         # pace prediction
         pace_pred = nm(str_extract(pacemapdata, '\\d+$')),
         # win vars
         win=ifelse(positionofficial == 1, 1, 0),
         # market implied prob
         bsp_prob=(1/betfairwinsp),
         run1_bsp_prob=1/run1_betfairwinsp,
         # bsp run1 to run0 bsp fluc
         run1_betfairwinsp = ifelse(is.na(run1_betfairwinsp)|run1_betfairwinsp==0, betfairwinsp, run1_betfairwinsp),
         bsp_run1_fluc = (run1_betfairwinsp-betfairwinsp)/run1_betfairwinsp,
         bsp_run1_fluc = ifelse(bsp_run1_fluc < -2, -2, bsp_run1_fluc),
         bsp_prob_run1_fluc = (bsp_prob - run1_bsp_prob),
         bsp_prob_run1_fluc = ifelse(is.na(bsp_prob_run1_fluc), 0, bsp_prob_run1_fluc)) %>%
  # race-specific vars
  group_by(raceid) %>%
  mutate(fave_price=min(bsp_5),
         kerdos_rating = pre_race_rating_adjusted, # kerdos rating normalised
         kerdos_rating_z = as.numeric(scale(pre_race_rating_adjusted)), # z-value
         # add k-rating rank per race here (after any race filtering) 
         kerdos_rating_rank=rank(-kerdos_rating, ties.method="min")
         ) %>% 
  ungroup() %>%
  # attach jockey/trainer ratings
  left_join(., jockey_ratings %>% select(jockeyfullname=Jockey, Period, Rating=RATING), 
            by=c('jockeyfullname')) %>%
  left_join(., trainer_ratings %>% select(trainerfullname=Trainer, Period, Rating=OVERALL), 
            by=c('trainerfullname'), suffix=c('_Jockey', '_Trainer')) %>%
  # join minimum jockey rating to replace NA's
  left_join(., jockey_rating_mins, by='jockeyfullname') %>%
  mutate(jockey_rating = ifelse(is.na(Rating_Jockey), Rating_Jockey_min, Rating_Jockey),
         jockey_rating_na = as.factor(ifelse(is.na(jockey_rating), 1, 0))) %>%
  mutate(jockey_rating_bin=round(Rating_Jockey/10),
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
           TRUE ~ 3),
         # market data calcs
         bf_prob_60 = 1/bsp_60,
         bf_prob_5 = 1/bsp_5,
         bf_prob_60_5 = (bf_prob_5 - bf_prob_60)/bf_prob_60,
         bf_prob_run1_fluc = (bf_prob_5 - run1_bsp_prob)/run1_bsp_prob,
         bf_prob_run1_fluc = ifelse(is.na(bf_prob_run1_fluc), 0, bf_prob_run1_fluc),
  ) %>%
  group_by(raceid) %>%
  mutate(bf_prob_60_rank = rank(bsp_60, ties.method="min")) %>%
  ungroup() %>%
  # factor vars
  mutate(across(all_of(factor_vars), ~as.factor(.))) %>%
  # join class data
  left_join(., class_rankings_old %>% select(raceid, category_rank, category_blended_score), by='raceid') %>%
  left_join(., class_ranking_groups_fill, by=c('racecode', 'raceclass', 'age_category')) %>% 
  mutate(category_rank=ifelse(is.na(category_rank), rank_median, category_rank),
         category_blended_score=ifelse(is.na(category_blended_score), blended_score_ave, category_blended_score)) %>%
  # select relevant cols
  select(race_vars, model_vars) %>%
  filter(between(bsp_5, 1.1, 50)) %>%
  arrange(scheduledtimeofracegmt, saddlecloth)



# run test winprobs to match software winprobs to
winprobs <- predict(final_model, gb_race_test_data, type = "prob")


# test output file with winprobs 
gb_race_test_preds <- gb_race_test_data %>%
  # attach winprobs
  bind_cols(winprobs[1]) %>%
  rename(winprob=.pred_Win) %>%
  # sum(winprob)=1.0
  group_by(raceid) %>%
  mutate(winprob_adj=winprob/sum(winprob),
         winprice=1/winprob,
         winprice_adj=1/winprob_adj) %>%
  ungroup()


