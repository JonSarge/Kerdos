#################################################
# Mike Spence
# UK Horse Racing Model - National Hunt (Jumps)
#
# commenced: 28.10.2026; updated: 28.10.2026
#
# Actions:
#  - data processing
#  - feature engineering
#  - multinomial logistic regression & xgboost models
#  - model accuracy comparison
#  - PnL report (flat-stake) 
#  - bet strategy
#
# Ask Mike:
#  - what markets (place betting?)
#  - opening fixed prices?
#  - other project objectives?
#  - are all features accessible on raceday?
#################################################

library(dplyr)
library(stringr)
library(tidyverse)
library(vip)
library(probably)
library(themis)
library(RMariaDB)
library(slider)
library(gert)

ch=function(x){as.character(x)}
fct=function(x){as.factor(x)}
nm=function(x){as.numeric(x)}
options(dplyr.summarise.inform = FALSE); options(scipen=99)


#### LOAD SOURCE DATA from wd ####


# load relevant raw Kerdos data (pre-feature engineering)
if(!exists('gb_racing_sql')){
  # SQL connection
  kon <- dbConnect(
    MariaDB(),
    user     = "avnadmin",
    password = "AVNS_hiTM-DHel0T90WLNRQx",
    dbname   = "master-db",
    host     = "mysql-20d0ecf2-jonsargent15-c8e7.c.aivencloud.com",
    port     = 24081
  )
  # get sql data
  gb_racing_sql <- dbGetQuery(kon, "select distinct * from gb_racing_data where scheduledtimeofracegmt >= '2024-01-01'")
}

# load csv with all data then cull cols for final rda save
# gb_racing_data_nh_all <- read.csv('gb_racing_data_nh.csv')
# gb_racing_data_nh <- gb_racing_data_nh_all %>% select(names(gb_racing_sql), bsp_60, bsp_10, bsp_5, sb_o, sb_m) 
# save(gb_racing_data_nh, file='gb_racing_data_nh.rda')

# load raw data
load('gb_racing_data_nh.rda')

#load flucs data
#load("kerdos_flucs_data.rda")

# load recent raw data from Mike and select relevant (stored) cols only
#gb_data_recent_1 <- read.csv("gb_recent_data.csv")

# market_data_recent <- gb_data_recent_1 %>%
#   select(names(flucs_data))

#flucs_data <- flucs_data %>% bind_rows(., market_data_recent)

# final recent data to be rbinded
# gb_data_recent <- gb_data_recent_1 %>%
#   select(names(gb_racing_sql)) %>%
#   mutate(raceid=ch(raceid))


#### FEATURE ENGINEERING #####

# loadNewData = 0

# price intervals
bsp_int=c(seq(0, 10, 2.5), 15, 25, 50, 1001)

# jockey/trainer ratings data
jockey_ratings <- read.csv('jockey_ratings.csv')
trainer_ratings <- read.csv('trainer_ratings.csv')

# use mins to fill na's not in a jockey period
jockey_rating_mins <- jockey_ratings %>% 
  group_by(jockeyfullname=Jockey) %>% 
  summarise(Rating_Jockey_min=min(RATING)) %>% 
  ungroup()


#### market data prep - join to pre_model_data below ####


# bsp data
bsp_data_old <- flucs_data %>%
  filter(!is.na(horsecode)) %>%
  select(raceid, scheduledtimeofracegmt, horsecode, bsp_60, bsp_10, bsp_5)
# extract market $ data from recent
bsp_data_recent <- gb_data_recent_1 %>%
  select(raceid, scheduledtimeofracegmt, horsecode, bsp_60, bsp_10, bsp_5)
# bind old & new
bsp_data <- rbind(bsp_data_old, bsp_data_recent)
# check dupes
bsp_data %>% count(raceid, horsecode) %>% filter(n>1)
# check NA's
bsp_60_na <- bsp_data %>% filter(is.na(bsp_60))

# bookies data
bookies_data_1 <- flucs_data %>%
  filter(!is.na(horsecode)) %>%
  select(raceid, scheduledtimeofracegmt, horsecode, starts_with('sb_'))
# updated data
bookies_data_2 <- read.csv('sb_output.csv') %>% select(-c(meetingdate, coursename))
# join and pre old data
bookies_data_old <- bookies_data_1 %>%
  left_join(., bookies_data_2, by=c('raceid', 'horsecode'), suffix=c('', '_1')) %>%
  mutate(sb_o = coalesce(sb_o_1, sb_o),
         sb_m = coalesce(sb_m_1, sb_m),
         sb_m=ifelse(is.na(sb_m), sb_o, sb_m)) %>%
  select(-ends_with('_1'))
# get recent data
bookies_data_recent <- gb_data_recent_1 %>%
  select(raceid, scheduledtimeofracegmt, horsecode, starts_with('sb_')) %>%
  mutate(sb_m=ifelse(is.na(sb_m), sb_o, sb_m))
# join old & recent
bookies_data <- rbind(bookies_data_old, bookies_data_recent) %>% select(-scheduledtimeofracegmt)
# check for dupes
bookies_data %>% count(raceid, horsecode) %>% filter(n>1)
# check NA's
bookies_na <- bookies_data %>% filter(is.na(sb_m))

# rbind this to flucs data old
market_data <- bsp_data %>% unique() %>%
  left_join(., bookies_data, by=c('raceid', 'horsecode'))
  
  



#### class rank data prep ####

# class ranks
class_rankings_old <- read.csv('gb_racing_raceclass.csv')
# class rank group summary
class_ranking_groups <- read.csv("class_ranks.csv", comment.char="#", 
                        fileEncoding = "latin1")

class_ranking_groups_fill <- class_ranking_groups %>%
  group_by(race_type, raceclass, age_category) %>%
  mutate(blended_score_ave=mean(blended_score),
         rank_median=median(rank),
         raceclass=as.factor(raceclass)) %>%
  ungroup() %>%
  select(racecode, raceclass, age_category, rank_median, blended_score_ave) %>%
  unique()

class_data <- gb_data_recent %>%
  select(raceid, racecode, raceclass, horseage, horsegender) %>%
  arrange(horsegender) %>%
  group_by(raceid) %>%
  mutate(gender=list(unique(horsegender))) %>% ungroup() %>%
  mutate(gender = sapply(gender, paste, collapse = ", "),
         race_type = case_when(
           racecode == "E"  ~ "Claimer",
           racecode == "G"  ~ "Maiden",
           racecode == "G1" ~ "Group 1",
           racecode == "G2" ~ "Group 2",
           racecode == "G3" ~ "Group 3",
           racecode == "GA" ~ "Maiden",
           racecode == "HM" ~ "Handicap",
           racecode == "I"  ~ "Handicap 1",
           racecode == "IA" ~ "Handicap (Apprentice)",
           racecode == "IM" ~ "Handicap (Amateur)",
           racecode == "L"  ~ "Listed",
           racecode == "P"  ~ "Novice",
           racecode == "PA" ~ "Novice",
           racecode == "S"  ~ "Handicap",
           racecode == "W"  ~ "Seller",
           racecode == "X"  ~ "Claiming Handicap",
           racecode == "XA" ~ "Handicap",
           TRUE ~ NA_character_
         ),
         )

gender_combos <- class_data %>% count(gender)




# get GB Flat Races - bind all years here!
gb_jumps_all_cols <- gb_racing_data_nh %>% 
  
  unique() %>%
  
  # append recent data
 # {if (loadNewData == 1) bind_rows(., gb_data_recent) else .} %>%
  
  # primary model filters
  filter(countrycode=='GBR',
         racetype %in% c('Chase', 'Hurdle'),
         positionofficial > 0 # remove DNF
  ) %>%
  
  unique() %>%
  
  # add relevant vars (win, profit etc)
  mutate(meetingdate=as.Date(scheduledtimeofracegmt),
         year=year(meetingdate),
         month=month(meetingdate, label = TRUE, abbr = TRUE),
         # 6-monthly Period (for ratings)
         Period = case_when(
           meetingdate >= "2023-07-01" & meetingdate <= "2023-12-31" ~ 0,
           meetingdate >= "2024-01-01" & meetingdate <= "2024-06-30" ~ 1,
           meetingdate >= "2024-07-01" & meetingdate <= "2024-12-31" ~ 2,
           meetingdate >= "2025-01-01" & meetingdate <= "2025-06-30" ~ 3,
           meetingdate >= "2025-07-01" & meetingdate <= "2025-12-31" ~ 4,
           meetingdate >= "2026-01-01" & meetingdate <= "2026-06-30" ~ 5,
           meetingdate >= "2026-07-01" & meetingdate <= "2026-12-31" ~ 6,
           TRUE ~ NA_real_
         ),
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
         # relative draw (to field sixze)
         draw_relative = (draw - 1) / (numberofrunners - 1),
         # binned relative draw
         draw_bin = case_when(
           draw_relative <= 0.1 ~ 'Rail',
           draw_relative > 0.1 & draw_relative <= 0.33 ~ 'Inside',
           draw_relative > 0.33 & draw_relative <= 0.67 ~ 'Middle',
           draw_relative > 0.67 & draw_relative <= 0.9 ~ 'Outside',
           TRUE ~ 'Wide'
         ),
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
         # class changes
         class_prior = case_when(
           raceclass > run1_raceclass ~ 'Down',
           raceclass < run1_raceclass ~ 'Up',
           raceclass == run1_raceclass ~ 'Same',
           TRUE ~ NA  
         ),
         # Create relative weight feature
         weight_relative = scale(weightcarried)[,1],
         # convert distance to meters
         distance_m=distance*201.168,
         #daysoff adj
         daysoff = ifelse(is.na(daysoff), 0, daysoff),
         # pace prediction
         pace_pred = nm(str_extract(pacemapdata, '\\d+$')),
         # win vars
         win=ifelse(positionofficial == 1, 1, 0),
         win_place=ifelse(positionofficial %in% c(1, 2, 3), 1, 0),
         # profit for roi
         profit=win*betfairwinsp-1,
         # bsp intervals
         bsp_index=cut(betfairwinsp, bsp_int, right=F, labels=F),
         bsp_bin=gsub(',','-', gsub('\\)', '\\]', cut(betfairwinsp, bsp_int, right=F))),
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
  mutate(fave_price=min(ispdecimal)) %>% 
  ungroup() %>%
  arrange(scheduledtimeofracegmt)


# dupe records
pre_model_data_dupes <- gb_jumps_all_cols %>%
  group_by(raceid, horsecode) %>%
  filter(n()>1) %>% ungroup()
if(nrow(pre_model_data_dupes)>0) cat("CHECK RUNNER DUPES!!!")


#### ratings analysis ####

# remove TF missing ratings (0, 999)
# Where a Timeform rating in our dataset is 0, it is correct and the Timeform rating is 0 in the API
# This is where a horse is having its first run in a race type, hasn’t run in this race type for a long time OR there is not enough data for Timeform to go on

race_vars <- c('raceid', 'racenumber', 'meetingdate', 'scheduledtimeofracegmt', 'coursename', 
               'horsecode', 'horsename', 'positionofficial', 'numberofrunners', 'betfairwinsp', 
               'bsp_index', 'bsp_bin', 'raceclass', 'run1_raceclass', 'racecode', 'racesurfacename')

# remove these races below for clean data to estimate missing K from TF
rating_anomalies <- gb_jumps_all_cols %>%
  mutate(rating_diff_perc = (pre_race_rating_adjusted-preraceadjustedrating)/preraceadjustedrating) %>%
  filter(abs(rating_diff_perc) > 0.5 |
         preraceadjustedrating %in% c(0, 999)) %>%
  select(raceid, rating_diff_perc, pre_race_rating_adjusted, preraceadjustedrating, 
         ends_with(c('post_race_rating_adjusted', 'performancerating')))

# ratings data w/o anomalies
rating_dat <- gb_jumps_all_cols %>%
  filter(!raceid %in% rating_anomalies$raceid) %>%
  select(all_of(race_vars), betfairwinsp, positionofficial, pre_race_rating_raw,
         contains(c('rating','figure', 'trw', 'Rating'))) # , 'figure', 'trw', 'Rating' 

# Kerdos v Timeform
kerdos_ratings <- rating_dat %>% select(all_of(race_vars), pre_race_rating_adjusted, contains('post_race'), pre_race_rating_raw, contains('postrace'), -ends_with('_symbol')) %>% unique()
tf_ratings <- rating_dat %>% select(all_of(race_vars), contains(c('adjustedrating', 'masterrating', 'performancerating', 'timefigure'))) %>% unique()
# missing values model from this df
both_ratings <- kerdos_ratings %>%
  select(-starts_with('run')) %>%
  left_join(., tf_ratings %>% # attach tf ratings
            select(raceid, horsecode, preraceadjustedrating:performancerating, timefigure),
            by=c('raceid', 'horsecode')) %>%
  filter(complete.cases(.),
         pre_race_rating_adjusted != 0) # filter out any NA's for NA modelling only

# ratings correlations
corr_matrix <- both_ratings %>% select(contains(c('rating', 'timefigure'))) %>% cor(.)

# CHECK: r'ship between Kerdos & Timeform pre-race ratings
plot(both_ratings$preraceadjustedrating, both_ratings$pre_race_rating_adjusted,
     main='Kerdos-Timeform Relationship', xlab='Timeform', ylab="Kerdos")

# NA prediction (linear regression) - estimate missing Kerdos ratings in model df
kerdos_est_model <- lm(pre_race_rating_adjusted ~ preraceadjustedrating, data=both_ratings)
summary(kerdos_est_model)

# CHECK: ratings dist'na
hist(kerdos_ratings$pre_race_rating_adjusted, main='Kerdos Master Rating', xlab='Rating')
hist(tf_ratings$preraceadjustedrating, main='Timeform Master Rating', xlab='Rating')


#### Feature engineering ####

# factor vars
factor_vars = c('racenumber', 'draw', 'draw_bin', 'positionofficial', 'horsegender', 'distancetype', 'racetype', 'raceclass',  
                'racesurfacename',  'racecode', 'going',  'iphintsoverallpace', 'pacemapdata', 'field_size',
                'jockeycode', 'jockeyfullname', 'trainercode', 'trainerfullname', 'ratingstars', 'rating123', 'significantimprover', 'jockeyinform', 
                'toprated', 'hottrainer', 'eligibilitysexlimit', 'preracemastersymbol', 'hotrace', 'pre_race_rating_symbol_for_16'
                )  

# CHECK all NA/Zero columns
na_zero_cols_df <- gb_jumps_all_cols %>% select(raceid, scheduledtimeofracegmt, coursename, where(~all(.==0, na.rm=TRUE)))
na_cols <- na_zero_cols_df %>% summarise_all(~all(is.na(.))) %>% select_if(~.) %>% names()
print(na_cols)
zero_cols <- na_zero_cols_df %>% summarise_all(~all(.==0)) %>% select_if(~.) %>% names()
print(zero_cols)

kerdos_rating_preds = predict(kerdos_est_model, gb_jumps_all_cols)

#sim_ints <- c(120, 60, 10, 5, 0) # market price int's
sim_ints <- c(5)
i = 1

# generate pre-model df with ratings adj's
pre_model_data <- gb_jumps_all_cols %>%
   # barrier pos e.g. 1_3 = inside, 10+ = wide etc
  mutate(across(all_of(factor_vars), ~fct(.))) %>%
  # ESTIMATE MISSING K/TF RATINGS:
  mutate(kerdos_rating = case_when(  # kerdos rating w/ estimates for NA's
           is.na(pre_race_rating_adjusted) & !preraceadjustedrating %in% c(0,999) ~ kerdos_rating_preds, # Kerdos NA's
           pre_race_rating_adjusted < 20 & between(preraceadjustedrating, 40, 200) ~ kerdos_rating_preds,
           TRUE ~ pre_race_rating_adjusted
         )
  ) %>%
  # Scale ratings per field (try normalising by raceclass @ venue???)
  group_by(raceid) %>%
  mutate(# kerdos rating normalised
         kerdos_rating_z = nm(scale(kerdos_rating)), # z-value
         # scaled to 0-100
         kerdos_rating_adj = pnorm(kerdos_rating_z) * 100, # 0-100 adj
         # add k-rating rank per race here (after any race filtering) 
         kerdos_rating_rank=rank(-kerdos_rating, ties.method="min"),
         bsp_rank=rank(betfairwinsp, ties.method="min"),
         ) %>% ungroup() %>%
  relocate(contains('kerdos_rating'), .after=pre_race_rating_adjusted) %>%
  select(!c(all_of(na_cols), all_of(zero_cols))) %>%
  # attach jockey/trainer ratings
  #left_join(., jockey_ratings %>% select(jockeyfullname=Jockey, Period, Rating=RATING), 
  #          by=c('jockeyfullname', 'Period')) %>%
  #left_join(., trainer_ratings %>% select(trainerfullname=Trainer, Period, Rating=OVERALL), 
  #          by=c('trainerfullname', 'Period'), suffix=c('_Jockey', '_Trainer')) %>%
  # join minimum jockey rating to replace NA's
  #left_join(., jockey_rating_mins, by='jockeyfullname') %>%
  #mutate(jockey_rating = ifelse(is.na(Rating_Jockey), Rating_Jockey_min, Rating_Jockey),
  #       jockey_rating_na=as.factor(ifelse(is.na(Rating_Jockey), 1, 0))) %>%
  # join flucs data
  #left_join(., market_data %>% select(-scheduledtimeofracegmt), by=c('raceid', 'horsecode')) %>%
  unique() %>%
  # market features
  mutate(bsp_5=ifelse(is.na(bsp_5), bsp_60, bsp_5), # correction for a few blank bsp-5 prices
         bf_prob_60 = 1/bsp_60,
         bf_prob_10 = 1/bsp_10,
         bf_prob_5 = 1/bsp_5,
         bf_prob_0 = 1/betfairwinsp,
         bf_prob_60_5 = (bf_prob_5 - bf_prob_60),
         bf_prob_5_run1_fluc = (bf_prob_5 - run1_bsp_prob)/run1_bsp_prob,
         bf_prob_5_run1_fluc = ifelse(is.na(bf_prob_5_run1_fluc), 0, bf_prob_5_run1_fluc),
         bf_prob_10_run1_fluc = (bf_prob_10 - run1_bsp_prob)/run1_bsp_prob,
         bf_prob_10_run1_fluc = ifelse(is.na(bf_prob_10_run1_fluc), 0, bf_prob_10_run1_fluc),
         bf_prob_60_run1_fluc = (bf_prob_60 - run1_bsp_prob)/run1_bsp_prob,
         bf_prob_60_run1_fluc = ifelse(is.na(bf_prob_60_run1_fluc), 0, bf_prob_60_run1_fluc),
         bf_under_5 = fct(ifelse(bsp_60 < 5, 1, 0)),
         age_category=ifelse(horseage <= 2, '2yo only', 'Older (3yo+, ages combined)')) %>%
  group_by(raceid) %>%
  mutate(bf_prob_60_rank = rank(bsp_60, ties.method="min"),
         runners_under_5=sum(bsp_60<5),
         raceclass_5_6=fct(ifelse(raceclass %in% c(5, 6), 1, 0))) %>% ungroup() %>%
    # join class data
  #left_join(., class_rankings_old %>% select(raceid, category_rank, category_blended_score), by='raceid') %>%
  #left_join(., class_ranking_groups_fill, by=c('racecode', 'raceclass', 'age_category')) %>% 
  mutate(#category_rank=ifelse(is.na(category_rank), rank_median, category_rank),
         # category_blended_score=ifelse(is.na(category_blended_score), blended_score_ave, category_blended_score),
         # jockey_rating_bin=round(jockey_rating/10),
         # jockey_rating_bin=ifelse(is.na(jockey_rating_bin), 5, jockey_rating_bin),
         # jockey_rating_top=case_when(
         #   jockey_rating_bin %in% c(9:10) ~ 1,
         #   jockey_rating_bin %in% c(5:8) ~ 2,
         #   jockey_rating_bin %in% c(1:4) ~ 3,
         #   TRUE ~ 3),
         # trainer_rating_bin=round(Rating_Trainer/10),
         # trainer_rating_bin=ifelse(is.na(trainer_rating_bin), 5, trainer_rating_bin),
         # trainer_rating_top=case_when(
         #   trainer_rating_bin %in% c(8:10) ~ 1,
         #   trainer_rating_bin %in% c(5:7) ~ 2,
         #   trainer_rating_bin %in% c(1:4) ~ 3,
         #   TRUE ~ 3),
         # adjust bfp_prob_60 for prices > 60 mins out
         bf_prob_60 = case_when(
          sim_ints==120 ~ 1/sb_m, 
          TRUE ~ bf_prob_60
        ),
        # get relevant bf price @ int i
        bf_price = case_when(
          sim_ints[i]==120 ~ sb_m * 1.025,
          sim_ints[i]==60 ~ bsp_60,
          sim_ints[i]==10 ~ bsp_10,
          sim_ints[i]==5 ~ bsp_5,
          TRUE ~ betfairwinsp
        ),
        # get relevant bf prob @ int i
        bf_prob = 1 / bf_price, 
        # adj run1 fluc for bf prob
        bf_prob_run1_fluc = (bf_prob - run1_bsp_prob)^2,
        bf_prob_run1_fluc = ifelse(is.na(bf_prob_run1_fluc)|is.infinite(bf_prob_run1_fluc), 0, bf_prob_run1_fluc),
        # adjust profit for bf price
        betfairwinsp = bf_price # bsp_0 * 0.0 + bsp_60 * 1.0
   ) %>%
   # adj fave price for bf price
   group_by(raceid) %>%
   mutate(fave_price=min(bf_price)) %>% 
   ungroup() %>%
   #mutate(across(c(jockey_rating_top, trainer_rating_top), ~as.factor(.))) %>%
  arrange(scheduledtimeofracegmt, saddlecloth)


# rating anomalies to be removed
ratings_remove <- pre_model_data %>% 
  #mutate(rating_diff_perc = (kerdos_rating-preraceadjustedrating)/preraceadjustedrating) %>% 
  filter(is.na(kerdos_rating)|
         ((preraceadjustedrating %in% c(0,999) & !between(kerdos_rating, 30, 200)) &
         !(kerdos_rating < 30 & between(preraceadjustedrating, 30, 200)))
         ) %>%
  select(all_of(race_vars), pre_race_rating_adjusted, kerdos_rating, preraceadjustedrating, 
         ends_with(c('post_race_rating_adjusted', 'performancerating')))


# check jockey/trainer NA's
jockey_trainer_dat <- pre_model_data %>%
  select(raceid, meetingdate, period=Period,
         coursename, racenumber, raceclass, racecode, numberofrunners, bsp=betfairwinsp, jockeyfullname, 
         trainerfullname, starts_with('Rating_'), jockey_rating, jockey_rating_na, -kerdos_rating_rank) 

jockey_nas <- jockey_trainer_dat %>%
  #filter(period==5) %>%
  group_by(raceid, coursename, raceclass, racecode, numberofrunners) %>%
  summarise(across(jockey_rating, ~sum(is.na(.x))),.groups = "drop_last") %>%
  rename(jockey_rating_nas=jockey_rating) %>%
  filter(jockey_rating_nas > 0)


jockey_nas <- jockey_trainer_dat %>%
  #filter(period==5) %>%
  group_by(raceid, coursename, raceclass, racecode, numberofrunners) %>%
  summarise(across(Rating_Jockey, ~sum(is.na(.x))),.groups = "drop_last") %>%
  #filter(numberofrunners==Rating_Jockey) %>%
  rename(jockey_rating_nas=Rating_Jockey)

trainer_nas <- jockey_trainer_dat %>%
  #filter(period==5) %>%
  group_by(raceid, coursename, raceclass, racecode, numberofrunners) %>%
  summarise(across(Rating_Trainer, ~sum(is.na(.x))),.groups = "drop_last") %>%
  rename(trainer_rating_nas=Rating_Trainer)

bf_nas <- pre_model_data %>% 
  filter(is.na(bsp_60)) %>% 
  select(raceid, meetingdate, starts_with('bsp_')) %>%
  pull(raceid) %>% unique()


sb_data <- pre_model_data %>% 
  mutate(positionofficial=nm(ch(positionofficial)),
         sb_o = coalesce(sb_o_2, sb_o_1),
         sb_m = coalesce(sb_m_2, sb_m_1)) %>%
  #filter(is.na(sb_m)) %>% 
  select(raceid, meetingdate, coursename, saddlecloth, numberofrunners, positionofficial, 
         sb_o, sb_m, bf_60=bsp_60, bf_10=bsp_10, bf_5=bsp_5, bf_0=betfairwinsp) %>%
  unique()

sb_scratchings <- sb_data %>% 
  group_by(raceid) %>% 
  filter(sum(is.na(bf_60)) < numberofrunners) %>%
  mutate(scratchings = sum(is.na(positionofficial) | positionofficial == 0)) %>% ungroup() %>% 
  relocate(scratchings, .after=numberofrunners) %>% 
  filter(scratchings>=4 & numberofrunners > scratchings) %>% 
  arrange(raceid, positionofficial)

class_nas <- pre_model_data %>% filter(is.na(category_rank))


# check any k-rating na's
kerdos_rating_nas <- pre_model_data %>% filter(is.na(pre_race_rating_adjusted))
# raceclass na's
raceclass_nas <- pre_model_data %>% filter(is.na(raceclass))
# run1_raceclass na's
run1_raceclass_nas <- pre_model_data %>% filter(is.na(run1_raceclass), run1_meetingdate!='')
# bsp NAs
bsp_nas <- pre_model_data %>% filter(is.na(betfairwinsp))
# run1 bsp NAs
run1_bsp_nas <- pre_model_data %>% filter(is.na(run1_betfairwinsp))


# get col max string lengths - compare to sql
cols <- dbGetQuery(kon, "SHOW COLUMNS FROM gb_racing_data")
sql_lengths <- cols %>%
  mutate(max_length = as.integer(str_match(cols$Type, "\\((\\d+)\\)")[, 2])) %>%
  select(-c(Type:Extra))
string_lengths <- pre_model_data %>% 
  summarise(across(everything(), ~max(nchar(as.character(.)), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Field", values_to = "max_length") %>%
  left_join(., sql_lengths, by='Field', suffix=c('_R', '_SQL')) %>%
  filter(max_length_R > max_length_SQL)
if(nrow(string_lengths)>0) "UPDATE SQL STRING LENGTH MAX"


# attach new data
if(loadNewData == 1 & nrow(string_lengths)==0){
  # ids stored in sql
  stored_raceids <- dbGetQuery(kon, 'select distinct raceid from gb_racing_data')
  # append newly added data
  pre_model_data_sql <- pre_model_data %>% 
    filter(!raceid %in% stored_raceids$raceid) %>% 
    select(names(gb_racing_sql))
  # append new data
  if(nrow(pre_model_data_sql)>0) dbWriteTable(kon, name='gb_racing_data', value=pre_model_data_sql, append=T, row.names=F)
}

dbDisconnect(kon)

# 
# 
# # numeric vars
# numeric_vars = c('distance', 'draw_relative', 'distancecumulative', 'distancebeaten', 'ispdecimal', 'betfairwinsp', 'betfairplacesp', 'weightcarried', 
#                  'daysoff', 'lastrunflat', 'horseage', 'eligibilityagemin', 'bharating', 'bsp_run1_fluc',
#                  'wfa', 'apprenticeclaim', 'weightcarried', 'weight_adjustment', 'ratinglimitupper', 'preraceepf', 'pre_race_rating_raw', 
#                  'postrace_rating_raw', 'pre_race_rating_adjusted', 'preraceadjustedrating', 'preracemasterrating', 'kerdos_rating',
#                  'performancerating', 'ratingsadjustment', 'adj_figure', 'timefigure', 'bharating', 'tissuepricedecimal', 'rating0to100', 'numberofrunners', # 'weight_relative', 
#                  'winsatcourse', 'winsatdistance', 'winsatcoursedistance', 'prizefund'
# )
# 
# # prior run vars
# run1_vars = c('run1_raceid', 'run1_meetingdate', 'run1_coursename', 'run1_positionofficial', 'run1_raceclass', 'run1_racecode', 'run1_racetype',
#               'run1_ratinglimitupper', 'run1_prizefund', 'run1_racesurfacename', 'run1_betfairwinsp')
# 
# # leave these out
# excl_vars <- c('scheduledtimeofracelocal',	'racetitle',	'distancetext',	'distanceyards',	'distancefurlongs',	
#                'courseextraid',	'eligibilityagemax',	'ratinglimitlower',	'resultsstatus', 'trw1',	'trw2',	'trw3',	'trw4',	'trw5',	
#                'tfwf',	'positionstatus',	'ispfavtext',	'ownercode',	'ownerfullname', 'performancesymbol',	'tissuepricefractional',	
#                'statusdescription', 'statusid',	'betfairbackprice',	'crsdiswinfavtext',	'bfselectionid',	'sportsbookdecimalodds',	
#                'sportsbookfractionalodds', 'masterinplaysymbol',	'pre_race_rating_symbol',	'sirehistoricalrating',	
#                'sirehistoricalsymbol',	'sireratingdisplay', 'damname',	'damhistoricalrating',	'damhistoricalsymbol',	'damratingdisplay',	
#                'damsirename',	'damsirehistoricalrating', 'damsirehistoricalsymbol',	'damsireratingdisplay',	'foalingdate',	'sirecd',	
#                'pacemapsidealposition', 'ispfractional', 'postrace_rating_symbol', 'jockeyuplift', 'traineruplift', 'horsesforcoursepos', 
#                'horsesforcoursesneg', 'warninghorse', 'bsp_run1_fluc')

# send season data back to repo
# git_add("uk_racing_data.R")
# git_commit("Update data script")
# git_push()

# remove run2+ for now
#select(-c(run2_raceid:run16_k_performance_rating_estimate_adj)) %>%
#relocate(starts_with('run1'), .after=pre_race_rating_diff) %>%
# final col selections
#select(courseid:run1_raceid, run1_meetingdate, run1_coursename, run1_draw, run1_betfairwinsp, 
#       run1_racetype, run1_raceclass, run1_racesurfacename, run1_racecode, run1_k_performance_rating_estimate_adj,
#       -all_of(excl_vars)) %>%
