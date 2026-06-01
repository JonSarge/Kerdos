#################################################
# Mike Spence
# UK Horse Racing Model
#
# commenced: 03.06.2025; updated: 03.06.2026
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

cdir = 'C:/Users/jonsa/OneDrive/STATS_ON/Projects/Mike_Spence/Kerdos'
#cdir = 'C:/Users/jonathan.sargent/Documents/Kerdos'
setwd(cdir)

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

# SQL connection
if ("package:RMySQL" %in% search()) {
  detach("package:RMySQL", unload = TRUE)
}
con <- dbConnect(
  MariaDB(),
  user     = "avnadmin",
  password = "AVNS_hiTM-DHel0T90WLNRQx",
  dbname   = "master-db",
  host     = "mysql-20d0ecf2-jonsargent15-c8e7.c.aivencloud.com",
  port     = 24081
)


#### LOAD SOURCE DATA from wd ####

# FULL raw Kerdos data set
  # load('gb_racing_2022_2026.rda')

# load relevant raw Kerdos data (pre-feature engineering)
gb_racing_sql <- dbGetQuery(con, "select distinct * from gb_racing_data where scheduledtimeofracegmt >= '2024-01-01'")

# load recent raw data from Mike and select relevant (stored) cols only
gb_data_recent <- read.csv("gb_recent_data.csv") %>% select(names(gb_racing_sql))


#### FEATURE ENGINEERING #####

loadNewData = 0

# price intervals
bsp_int=c(seq(0, 10, 2.5), 15, 25, 50, 1001)

# get GB Flat Races - bind all years here!
gb_flat_all_cols <- gb_racing_sql %>%
  
  # append recent data
  {if (loadNewData == 1) bind_rows(., gb_data_recent) else .} %>%
  
  # primary model filters
  filter(countrycode=='GBR',
         racetype=='Flat',
         positionofficial != 0 # remove DNF
  ) %>%
  
  # add relevant vars (win, profit etc)
  mutate(meetingdate=as.Date(scheduledtimeofracegmt),
         year=year(scheduledtimeofracegmt),
         # target variable
         finish_group = case_when(
           positionofficial == 1 ~ "Win",
           positionofficial %in% 2:3 ~ "Place",
           positionofficial %in% 4:6 ~ "Unplaced", 
           TRUE ~ "Out_of_Money"
         ),
         finish_group = factor(finish_group, levels = c("Win", "Place", "Unplaced", "Out_of_Money")),
         # Create relative weight feature
         weight_relative = scale(weightcarried)[,1],
         # pace prediction
         pace_pred = nm(str_extract(pacemapdata, '\\d+$')),
         # class diff - fix NA's in run1_raceclass with regression model
         class_prior = case_when(
           raceclass > run1_raceclass ~ 'Down',
           raceclass < run1_raceclass ~ 'Up',
           raceclass == run1_raceclass ~ 'Same',
           TRUE ~ NA  
         ),
         # convert distance to meters
         distance_m=distance*201.168,
         # win vars
         win=ifelse(positionofficial == 1, 1, 0),
         win_place=ifelse(positionofficial %in% c(1, 2, 3), 1, 0),
         # profit for roi
         profit=win*betfairwinsp-1,
         # bsp intervals
         bsp_index=cut(betfairwinsp, bsp_int, right=F, labels=F),
         bsp_bin=gsub(',','-', gsub('\\)', '\\]', cut(betfairwinsp, bsp_int, right=F))),
         # Log transform odds to reduce skewness
         bsp_log=log(betfairwinsp + 1),
         # market implied prob
         bsp_prob=1/betfairwinsp,
         # bsp run1 to run0 bsp fluc
         run1_betfairwinsp = ifelse(is.na(run1_betfairwinsp)|run1_betfairwinsp==0, betfairwinsp, run1_betfairwinsp),
         bsp_run1_fluc = (run1_betfairwinsp-betfairwinsp)/run1_betfairwinsp,
         bsp_run1_fluc = ifelse(bsp_run1_fluc < -2, -2, bsp_run1_fluc)
         # cap bsp @ $100???
         #bsp = ifelse(betfairwinsp >= 100, 100, betfairwinsp),
  ) %>%
  # race-specific vars
  group_by(raceid) %>%
  mutate(fave_price=min(ispdecimal)) %>% 
  ungroup() %>%
  arrange(scheduledtimeofracegmt)


# dupe records
pre_model_data_dupes <- gb_flat_all_cols %>%
  group_by(raceid, horsecode) %>%
  filter(n()>1) %>% ungroup()
if(nrow(pre_model_data_dupes)>0) cat("CHECK RUNNER DUPES!!!")


#### ratings analysis ####

# remove TF missing ratings (0, 999)
# Where a Timeform rating in our dataset is 0, it is correct and the Timeform rating is 0 in the API
# This is where a horse is having its first run in a race type, hasn’t run in this race type for a long time OR there is not enough data for Timeform to go on

race_vars <- c('raceid', 'racenumber', 'meetingdate', 'scheduledtimeofracegmt', 'coursename', 
               'horsecode', 'horsename', 'positionofficial')

# CHECK: uncorrelated kerdos & tf
rating_anomalies <- gb_flat_all_cols %>%
  mutate(rating_diff_perc = (pre_race_rating_adjusted-preraceadjustedrating)/preraceadjustedrating) %>%
  filter(abs(rating_diff_perc) > 0.5 |
          preraceadjustedrating %in% c(0, 999)) %>%
  select(raceid, horsecode, rating_diff_perc, pre_race_rating_adjusted, preraceadjustedrating, 
         ends_with(c('post_race_rating_adjusted', 'performancerating')))

# ratings data w/o anomalies
rating_dat <- gb_flat_all_cols %>%
  filter(!raceid %in% rating_anomalies$raceid) %>%
  select(all_of(race_vars), betfairwinsp, positionofficial, pre_race_rating_raw,
         contains(c('rating','figure', 'trw', 'Rating'))) # , 'figure', 'trw', 'Rating' 

# Kerdos v Timeform
k_ratings <- rating_dat %>% select(race_vars, pre_race_rating_adjusted, contains('post_race'), pre_race_rating_raw, contains('postrace'), -ends_with('_symbol')) %>% unique()
tf_ratings <- rating_dat %>% select(race_vars, contains(c('adjustedrating', 'masterrating', 'performancerating', 'timefigure'))) %>% unique()
# missing values model from this df
both_ratings <- k_ratings %>%
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
k_na_model <- lm(pre_race_rating_adjusted ~ preraceadjustedrating, data=both_ratings)
summary(k_na_model)
# estimate missing tf from k
tf_na_model <- lm(preraceadjustedrating ~ pre_race_rating_adjusted, data=both_ratings)
summary(tf_na_model)

# CHECK: ratings dist'na
hist(k_ratings$pre_race_rating_adjusted, main='Kerdos Master Rating', xlab='Rating')
hist(tf_ratings$preraceadjustedrating, main='Timeform Master Rating', xlab='Rating')


#### Feature engineering ####

# numeric vars
numeric_vars = c('distance', 'distancecumulative', 'distancebeaten', 'ispdecimal', 'betfairwinsp', 'betfairplacesp', 'weightcarried', 
                 'daysoff', 'lastrunflat', 'horseage', 'eligibilityagemin', 'bharating', 'bsp_run1_fluc',
                 'wfa', 'apprenticeclaim', 'weightcarried', 'weight_adjustment', 'ratinglimitupper', 'preraceepf', 'pre_race_rating_raw', 
                 'postrace_rating_raw', 'pre_race_rating_adjusted', 'preraceadjustedrating', 'preracemasterrating', 
                 'performancerating', 'ratingsadjustment', 'adj_figure', 'timefigure', 'bharating', 'tissuepricedecimal', 'rating0to100', 'numberofrunners', # 'weight_relative', 
                 'winsatcourse', 'winsatdistance', 'winsatcoursedistance', 'prizefund'
)

# prior run vars
run1_vars = c('run1_raceid', 'run1_meetingdate', 'run1_coursename', 'run1_positionofficial', 'run1_raceclass', 'run1_racecode', 'run1_racetype',
              'run1_ratinglimitupper', 'run1_prizefund', 'run1_racesurfacename', 'run1_betfairwinsp')

# factor vars
factor_vars = c('racenumber', 'draw', 'positionofficial', 'horsegender', 'distancetype', 'racetype', 'raceclass',  
                'racesurfacename',  'racecode', 'going',  'iphintsoverallpace', 'pacemapdata',
                'jockeycode', 'jockeyfullname', 'trainercode', 'trainerfullname', 'ratingstars', 'rating123', 'significantimprover', 'jockeyinform', 
                'toprated', 'hottrainer', 'eligibilitysexlimit', 'preracemastersymbol', 'hotrace', 'pre_race_rating_symbol_for_16'
) # 'pacemapsidealposition', 'pre_race_rating_symbol', 

# leave these out
excl_vars <- c('scheduledtimeofracelocal',	'racetitle',	'distancetext',	'distanceyards',	'distancefurlongs',	
               'courseextraid',	'eligibilityagemax',	'ratinglimitlower',	'resultsstatus', 'trw1',	'trw2',	'trw3',	'trw4',	'trw5',	
               'tfwfa',	'positionstatus',	'ispfavtext',	'ownercode',	'ownerfullname', 'performancesymbol',	'tissuepricefractional',	
               'statusdescription', 'statusid',	'betfairbackprice',	'crsdiswinfavtext',	'bfselectionid',	'sportsbookdecimalodds',	
               'sportsbookfractionalodds', 'masterinplaysymbol',	'pre_race_rating_symbol',	'sirehistoricalrating',	
               'sirehistoricalsymbol',	'sireratingdisplay', 'damname',	'damhistoricalrating',	'damhistoricalsymbol',	'damratingdisplay',	
               'damsirename',	'damsirehistoricalrating', 'damsirehistoricalsymbol',	'damsireratingdisplay',	'foalingdate',	'sirecd',	
               'pacemapsidealposition', 'ispfractional', 'postrace_rating_symbol', 'jockeyuplift', 'traineruplift', 'horsesforcoursepos', 
               'horsesforcoursesneg', 'warninghorse', 'bsp_run1_fluc')

# CHECK all NA/Zero columns
na_zero_cols_df <- gb_flat_all_cols %>% select(raceid, scheduledtimeofracegmt, coursename, where(~all(.==0, na.rm=TRUE)))
na_cols <- na_zero_cols_df %>% summarise_all(~all(is.na(.))) %>% select_if(~.) %>% names()
print(na_cols)
zero_cols <- na_zero_cols_df %>% summarise_all(~all(.==0)) %>% select_if(~.) %>% names()
print(zero_cols)


# generate pre-model df with ratings adj's
pre_model_data <- gb_flat_all_cols %>%
  # barrier pos e.g. 1_3 = inside, 10+ = wide etc
  mutate(across(all_of(factor_vars), ~fct(.))) %>%
  # ESTIMATE MISSUING K/TF RATINGS:
  mutate(k_rating_pred = predict(k_na_model, .),
         pre_race_rating_adjusted_y = round(ifelse(is.na(pre_race_rating_adjusted), k_rating_pred, pre_race_rating_adjusted)),
         tf_rating_pred = predict(tf_na_model, .)) %>%
  # Scale ratings per field (try normalising by raceclass @ venue???)
  group_by(raceid) %>%
  mutate(pre_race_rating_diff = pre_race_rating_adjusted_y - mean(pre_race_rating_adjusted_y),
         pre_race_rating_adjusted_z = nm(scale(pre_race_rating_adjusted_y)), # z-value
         pre_race_rating_adjusted_z_adj = pnorm(pre_race_rating_adjusted_z) * 100, # 0-100 adj
         ) %>% ungroup() %>%
  relocate(pre_race_rating_adjusted_z, pre_race_rating_adjusted_z_adj, .after=pre_race_rating_adjusted) %>%
  
  select(!c(na_cols, zero_cols)) %>%
  
  arrange(scheduledtimeofracegmt, saddlecloth)
  


# check any k-rating na's
k_rating_nas <- pre_model_data %>% filter(is.na(pre_race_rating_adjusted))
# raceclass na's
raceclass_nas <- pre_model_data %>% filter(is.na(raceclass))
# run1_raceclass na's
run1_raceclass_nas <- pre_model_data %>% filter(is.na(run1_raceclass), run1_meetingdate!='')
# bsp NAs
bsp_nas <- pre_model_data %>% filter(is.na(betfairwinsp))
# run1 bsp NAs
run1_bsp_nas <- pre_model_data %>% filter(is.na(run1_betfairwinsp))


# get col max string lengths - compare to sql
cols <- dbGetQuery(con, "SHOW COLUMNS FROM gb_racing_data")
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
  stored_raceids <- dbGetQuery(con, 'select distinct raceid from gb_racing_data')
  # append newly added data
  pre_model_data_sql <- pre_model_data %>% filter(!raceid %in% stored_raceids$raceid)
  # append new data
  if(nrow(pre_model_data_sql)>0){
    # write to sql
    dbWriteTable(con, name='gb_racing_data', value=pre_model_data_sql, append=T, row.names=F)
  }
}

# send season data back to repo
git_add("uk_racing_data.R")
git_commit("Update data script")
git_push()

# remove run2+ for now
#select(-c(run2_raceid:run16_k_performance_rating_estimate_adj)) %>%
#relocate(starts_with('run1'), .after=pre_race_rating_diff) %>%
# final col selections
#select(courseid:run1_raceid, run1_meetingdate, run1_coursename, run1_draw, run1_betfairwinsp, 
#       run1_racetype, run1_raceclass, run1_racesurfacename, run1_racecode, run1_k_performance_rating_estimate_adj,
#       -all_of(excl_vars)) %>%
