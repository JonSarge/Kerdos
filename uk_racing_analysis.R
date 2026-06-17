#################################################
# KERDOS
# UK Horse Racing Analysis
#
# commenced: 10.06.2026; updated: 10.06.2026
#
# Actions:
#  - perform ad hoc analysis to augment models
#
#################################################

library(dplyr)
library(stringr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(RMariaDB)

ch=function(x){as.character(x)}
fct=function(x){as.factor(x)}
nm=function(x){as.numeric(x)}
hms_times=function(x){format(as.POSIXct(x, origin="1970-01-01",tz="UTC"), '%H:%M')}

kon <- dbConnect(
  MariaDB(),
  user     = "avnadmin",
  password = "AVNS_hiTM-DHel0T90WLNRQx",
  dbname   = "master-db",
  host     = "mysql-20d0ecf2-jonsargent15-c8e7.c.aivencloud.com",
  port     = 24081
)

# raceclass NA fill
raceclass_dat <- tbl(kon, "gb_racing_data") |>
  select(raceid, scheduledtimeofracegmt, countrycode, coursename, racenumber, 
         racetype, racesurfacename, raceclass, racecode, prizefund, ratinglimitupper
         # run1_raceid, run1_meetingdate, run1_coursename, run1_racesurfacename,
         # run1_raceclass, run1_racecode, run1_racetype, run1_ratinglimitupper, 
         # run1_prizefund
         ) %>%
  filter(countrycode == 'GBR',
         racetype == 'Flat') %>%
  distinct() %>%
  collect() %>%
  mutate(across(c(raceclass, racecode), ~as.factor(.)))

raceclass_model_df <- raceclass_dat %>% 
  filter(!is.na(raceclass),
         !is.na(prizefund),
         is.finite(prizefund),   # Fix 2: drop Inf/-Inf rows
         racecode %in% c('I', 'P', 'G')) %>%
  mutate(prizefund_scaled = scale(prizefund)[, 1]) %>%
  slice_sample(n=1000)

train_dat <-  raceclass_model_df %>% slice_sample(prop = 0.8)
test_dat <- raceclass_model_df %>% filter(!raceid %in% train_dat$raceid)

model <- MASS::polr(raceclass ~ prizefund, data = train_dat, Hess = TRUE)


# raceclass rating limmits for coding NA's (no need for modelling)
raceclass_model_df  %>% 
  filter(!is.na(ratinglimitupper), ratinglimitupper>0) %>% 
  group_by(raceclass) %>% 
  summarise(n(), min(ratinglimitupper), max(ratinglimitupper))
  


# remove NA raceclass races with run1_prizefund=0 & run1_ratingupperlimit=0/NA
run1_raceclass_nas <- raceclass_dat %>% 
  select(contains('run1')) %>%
  filter(is.na(run1_raceclass),
         run1_raceid != '',
         run1_prizefund==0,
         (run1_ratinglimitupper==0|
          is.na(run1_ratinglimitupper))) 

# performancerating var to estimate missing run1_raceclass NA's
perfrating_dat <- gb_racing_2022_2026 %>% # import raw gb_racing_model_data.rda  
  filter(run1_meetingdate>='2023-01-01') %>%
  select(run1_raceid, run1_meetingdate, run1_coursename, run1_raceclass, 
         run1_performancerating) %>%
  group_by(run1_raceid) %>%
  mutate(run1_perfrating_mean=mean(na.omit(run1_performancerating))) %>% 
  ungroup()
  
perfrating_train <- perfrating_dat %>%
  filter(!run1_raceid %in% run1_raceclass_nas$run1_raceid,
         run1_raceclass>0) %>%
  mutate(run1_raceclass=fct(run1_raceclass)) %>%
  select(run1_raceclass, run1_perfrating_mean) %>%
  sample_n(size=1000)

model <- MASS::polr(run1_raceclass ~ run1_perfrating_mean, data = perfrating_train, Hess = TRUE)
summary(model)

perfrating_test <- perfrating_dat %>% 
  filter(run1_raceid %in% run1_raceclass_nas$run1_raceid,
         run1_perfrating_mean > 0) %>%
  select(-c(run1_performancerating, run1_raceclass)) %>%
  mutate(run1_raceclass_pred=predict(model, newdata = .))

  

# pull remaining NA's and model those with prizefund for current and prior run

# rcaeclass na's
# raceclass_nas_df <- model_data_w_nas %>% filter(is.na(run1_raceclass))
# # traindat
# raceclass_train_ids <- raceclass_nas_df %>% pull(run1_raceid) %>% unique()
# n <- length(raceclass_train_ids)
# train_idx <- sample(n, size = floor(0.7 * n))
# train_dat <- raceclass_nas_df %>% 
#   filter(run1_raceid %in% raceclass_train_ids[train_idx])
# test_dat  <- raceclass_nas_df %>% filter(!run1_raceid %in% train_dat$run1_raceid)