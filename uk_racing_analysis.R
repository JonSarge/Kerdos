#################################################
# KERDOS
# UK Horse Racing Analysis
#
# commenced: 10.06.2026; updated: 10.06.2026
#
# Actions:
#  - peroform ad hoc analysis to augment models
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

con <- dbConnect(
  MariaDB(),
  user     = "avnadmin",
  password = "AVNS_hiTM-DHel0T90WLNRQx",
  dbname   = "master-db",
  host     = "mysql-20d0ecf2-jonsargent15-c8e7.c.aivencloud.com",
  port     = 24081
)

# raceclass NA fill
raceclass_dat <- tbl(con, "gb_racing_data") |>
  select(raceid, scheduledtimeofracegmt, countrycode, coursename, racenumber, 
         racetype, racesurfacename, raceclass, racecode, prizefund, ratinglimitupper,
         run1_raceid, run1_meetingdate, run1_coursename, run1_racesurfacename,
         run1_raceclass, run1_racecode, run1_racetype, run1_ratinglimitupper, 
         run1_prizefund) %>%
  filter(scheduledtimeofracegmt >= "2024-01-01",
         countrycode == 'GBR',
         racetype == 'Flat') %>%
  distinct() %>%
  collect()

# filter na's by raceid
raceclass_nas <- raceclass_dat %>% filter(is.na(raceclass))

# impute L, G1-3 as C1!!!

# remove NA raceclass races with run1_prizefund=0 & run1_ratingupperlimit=0/NA

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