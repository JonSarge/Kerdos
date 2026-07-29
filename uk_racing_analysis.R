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

race_vars <- c('raceid', 'racenumber', 'scheduledtimeofracegmt', 'coursename', 
               'horsecode', 'horsename', 'positionofficial', 'numberofrunners', 'betfairwinsp', 
                'raceclass', 'run1_raceclass', 'racecode', 'racesurfacename')

race_vars_sql = paste(race_vars, collapse=',')

#### flucs analysis ####
race_data <- dbGetQuery(kon, paste0("select ", race_vars_sql, " from gb_racing_data where scheduledtimeofracegmt >= '2025-10-01' and scheduledtimeofracegmt <= '2025-12-31' ") )

loadFlucs = 0
if(loadFlucs==0){
  setwd('C:/Users/jonsa/Downloads')
  files <- c(
  'performance_data_2024-01-01_to_2024-04-30.csv',
  'performance_data_2024-05-01_to_2024-06-30.csv',
  'performance_data_2024-07-01_to_2024-09-30.csv',
  'performance_data_2024-10-01_to_2024-12-31.csv',
  'performance_data_2025-01-01_to_2025-04-30.csv',
  'performance_data_2025-05-01_to_2025-06-30.csv',
  'performance_data_2025-07-01_to_2025-09-30.csv',
  'performance_data_2025-10-01_to_2025-12-31.csv'
  )
  pr <- files %>% map_dfr(read_csv)
  setwd('C:/Users/jonsa/OneDrive/STATS_ON/Projects/Mike_Spence/Kerdos')
  
  flucs_data_csv <- pr %>% select(raceid, scheduledtimeofracegmt, horsecode, starts_with(c('bsp_', 'sb_')), -bsp_run1_fluc)
  flucs_cols <- c("bsp_60", "bsp_10", "bsp_5", "sb_o", "sb_m", "sb_2")
  
  save(flucs_data_csv, file='kerdos_flucs_data.rda')
}

load('kerdos_flucs_data.rda')

flucs_data_csv <- flucs_data_csv %>% 
  select(-c(scheduledtimeofracegmt)) %>%
  mutate(horsecode=nm(horsecode))

flucs_data <- model_data %>% 
  filter(meetingdate >= '2024-01-01' , meetingdate < '2025-12-31') %>%
  select(all_of(race_vars)) %>%
  left_join(., flucs_data_csv, by=c('raceid', 'horsecode')) %>%
  mutate(win=ifelse(positionofficial == 1, 1, 0)) %>%
  select(raceid, meetingdate, racesurfacename, raceclass, racecode, 
         horsename, win, bsp_60, bsp_10, bsp_5, bsp_0=betfairwinsp)
  # for NA's
  #filter(if_all(bsp_60:sb_2, is.na))

###############################################################################
# Price Fluctuation Model
# Hypothesis: if a horse's price shortens from bsp_60 (60 min before off) to
# bsp_0 (at the off), the likelihood of winning increases.
###############################################################################

library(tidyverse)
library(yardstick)
library(broom)

###############################################################################
# 0. LOAD DATA -- adjust this to however your data actually gets in
###############################################################################
# Assumes a data frame `flucs_df` with (at minimum): raceid, meetingdate, win,
# bsp_60, bsp_10, bsp_5, bsp_0. `win` is 0/1.

# flucs_df <- read_csv("your_file.csv")   # or however you're loading it

###############################################################################
# 1. FEATURE ENGINEERING: price movement / "steam"
###############################################################################
# Two natural ways to express "shortening":
#   - ratio:    bsp_60 / bsp_0   > 1 means the price shortened (odds fell)
#   - pct move: (bsp_60 - bsp_0) / bsp_60   positive means shortened
# pct move is easier to interpret directly ("shortened by X%"), so that's the
# primary feature below, computed at a few different horizons since a late
# steam move (bsp_5 -> bsp_0) might carry different information than an early
# drift (bsp_60 -> bsp_10).

flucs_df <- flucs_data %>%
  mutate(
    # overall move, 60 min out to the off
    pct_move_60_0 = (bsp_60 - bsp_0) / bsp_60,
    ratio_60_0    = bsp_60 / bsp_0,
    
    # split into two halves of the window, in case early vs late movement
    # carries different signal
    pct_move_60_10 = (bsp_60 - bsp_10) / bsp_60,
    pct_move_10_0  = (bsp_10 - bsp_0)  / bsp_10,
    
    # final implied probability, used as a control -- a short-priced horse
    # shortening slightly is different from a long shot shortening a lot, and
    # we want to know if movement predicts winning OVER AND ABOVE the final
    # price itself, not just re-discover "shorter price = more likely to win"
    bsp_0_prob = 1 / bsp_0
  ) %>%
  
  filter(bsp_0 <= 50, 
         #bsp_0 >= 2,
         racesurfacename == 'Turf',
         raceclass %in% c(1, 2, 3))

# quick sanity check: any non-positive or missing prices will break log-odds /
# ratios below -- inspect before modelling
flucs_df %>%
  summarise(
    n                = n(),
    n_missing_bsp60  = sum(is.na(bsp_60)),
    n_missing_bsp0   = sum(is.na(bsp_0)),
    n_nonpositive    = sum(bsp_60 <= 0 | bsp_0 <= 0, na.rm = TRUE)
  ) %>%
  print()

flucs_df <- flucs_df %>% filter(!is.na(pct_move_60_0), is.finite(pct_move_60_0))

###############################################################################
# 2-4. ANALYSIS FUNCTION -- decile check, models, AIC/AUC comparison
###############################################################################
# Wrapped in a function so it can be run once overall and once per surface
# (Turf vs All Weather), to check whether pooling the two masks a real effect
# in one of them.

auc_check <- function(model, data) {
  preds <- predict(model, newdata = data, type = "response")
  yardstick::roc_auc_vec(truth = factor(data$win, levels = c(1, 0)), estimate = preds)
}

run_price_analysis <- function(data, label) {
  
  print(paste0('###############################################################'))
  print(paste0('### SURFACE: ', label, '  (n = ', nrow(data), ' runners)'))
  print(paste0('###############################################################'))
  
  if(nrow(data) < 200) {
    print("WARNING: fewer than 200 rows -- results below may be unstable, interpret with caution")
  }
  
  print('=== WIN RATE BY DECILE OF PRICE MOVEMENT (bsp_60 -> bsp_0) ===')
  decile_check <- data %>%
    mutate(move_decile = ntile(pct_move_60_0, 10)) %>%
    group_by(move_decile) %>%
    summarise(
      n             = n(),
      mean_pct_move = mean(pct_move_60_0),
      win_rate      = mean(win),
      mean_bsp_0    = mean(bsp_0)
    ) %>%
    arrange(move_decile)
  print(decile_check)
  
  print('=== MODEL 1: win ~ price movement alone ===')
  model1 <- glm(win ~ pct_move_60_0, data = data, family = binomial())
  print(summary(model1)$coefficients)
  print(broom::tidy(model1, exponentiate = TRUE, conf.int = TRUE, conf.method = "Wald"))
  
  print('=== MODEL 2: win ~ price movement + final implied probability (control) ===')
  model2 <- glm(win ~ pct_move_60_0 + bsp_0_prob, data = data, family = binomial())
  print(summary(model2)$coefficients)
  print(broom::tidy(model2, exponentiate = TRUE, conf.int = TRUE, conf.method = "Wald"))
  
  print('=== MODEL 3: split into early vs late movement ===')
  model3 <- glm(win ~ pct_move_60_10 + pct_move_10_0 + bsp_0_prob, data = data, family = binomial())
  print(summary(model3)$coefficients)
  print(broom::tidy(model3, exponentiate = TRUE, conf.int = TRUE, conf.method = "Wald"))
  
  print('=== MODEL COMPARISON (AIC, lower is better) ===')
  print(tibble(
    model = c("movement only", "movement + final price", "early/late split + final price"),
    aic   = c(AIC(model1), AIC(model2), AIC(model3))
  ))
  
  baseline_price_only <- glm(win ~ bsp_0_prob, data = data, family = binomial())
  
  print('=== AUC COMPARISON ===')
  print(tibble(
    model = c("final price only (baseline)", "movement only", "movement + final price", "early/late split + final price"),
    auc   = c(
      auc_check(baseline_price_only, data),
      auc_check(model1, data),
      auc_check(model2, data),
      auc_check(model3, data)
    )
  ))
  
  invisible(list(model1 = model1, model2 = model2, model3 = model3, decile_check = decile_check))
}

###############################################################################
# RUN: overall, then split by surface
###############################################################################

print(paste("Surface values found in data:", paste(unique(flucs_df$racesurfacename), collapse = ", ")))

results_all <- run_price_analysis(flucs_df, "ALL (pooled)")

# adjust these labels if your actual values differ from "Turf"/"All Weather" --
# checked against the printed unique values above
surface_values <- unique(flucs_df$racesurfacename)

results_by_surface <- purrr::map(surface_values, function(surf) {
  run_price_analysis(flucs_df %>% filter(racesurfacename == surf), surf)
})
names(results_by_surface) <- surface_values

print('=== COMPLETE ===')



#### RACECLASS ANALYSIS ####

gb_racing_raceclass <- read.csv(
       "C:/Users/jonsa/OneDrive/STATS_ON/Projects/Mike_Spence/Kerdos/gb_racing_raceclass.csv",
       comment.char = "#",
       encoding = "latin1",
       fileEncoding = "latin1"
  ) %>%
  select(-c(horsename:postrace_rating_raw))


# raceclass NA fill
raceclass_dat <- tbl(kon, "gb_racing_data") |>
  select(raceid, scheduledtimeofracegmt, countrycode, coursename, racenumber, positionofficial,
         racetype, racesurfacename, raceclass, racecode, prizefund, ratinglimitupper
         ) %>%
  filter(countrycode == 'GBR',
         racetype == 'Flat',
         positionofficial > 0) %>%
  select(-positionofficial) %>%
  distinct() %>%
  collect() %>%
  mutate(across(c(raceclass, racecode), ~as.factor(.))) %>%
  left_join(., gb_racing_raceclass, by='raceid', suffix=c('', '_check'))

# check na's
raceclass_nas <- raceclass_dat %>% 
  filter(is.na(category_rank)) %>%
  select(raceid:racecode, contains('category'))

write.csv(raceclass_nas, 'raceclass_nas.csv')


  
  
  
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


#### jockey ratings analysis ####

jockey_ratings_csv <- read.csv('jockey_ratings.csv')

jockey_ratings_dat <- jockey_ratings_csv %>%
  mutate(
    period = case_when(
      Date >= "2023-07-01" & Date <= "2023-12-31" ~ 1,
      Date >= "2024-01-01" & Date <= "2024-06-30" ~ 2,
      Date >= "2024-07-01" & Date <= "2024-12-31" ~ 3,
      Date >= "2025-01-01" & Date <= "2025-06-30" ~ 4,
      Date >= "2025-07-01" & Date <= "2025-12-31" ~ 5,
      Date >= "2026-01-01" & Date <= "2026-06-30" ~ 6,
      Date >= "2026-07-01" & Date <= "2026-12-31" ~ 7,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-c(Period, Rank, Date)) %>%
  rename(jockeyfullname=Jockey) %>%
  relocate(Period=period, .before=RATING)
        
pre_model_jockey <- pre_model_data %>%
  mutate(Date=meetingdate,
         Period = case_when(
           Date >= "2023-07-01" & Date <= "2023-12-31" ~ 0,
           Date >= "2024-01-01" & Date <= "2024-06-30" ~ 1,
           Date >= "2024-07-01" & Date <= "2024-12-31" ~ 2,
           Date >= "2025-01-01" & Date <= "2025-06-30" ~ 3,
           Date >= "2025-07-01" & Date <= "2025-12-31" ~ 4,
           Date >= "2026-01-01" & Date <= "2026-06-30" ~ 5,
           Date >= "2025-01-01" & Date <= "2025-06-30" ~ 3,
           Date >= "2025-07-01" & Date <= "2025-12-31" ~ 4,
           Date >= "2026-01-01" & Date <= "2026-06-30" ~ 5,
           Date >= "2026-07-01" & Date <= "2026-12-31" ~ 6,
           TRUE ~ NA_real_
         )
  ) %>%
  left_join(., jockey_ratings_dat, by=c('jockeyfullname', 'Period'))

missing_jockeys <-pre_model_jockey %>%
  filter(is.na(Rating)) %>% 
  select(jockeycode, jockeyfullname, RATING) %>%
  unique()

# trainer rarting data
trainer_ratings_csv <- read.csv('trainer_ratings.csv')

trainer_ratings_dat <- trainer_ratings_csv %>%
  mutate(
    Date = dmy(Period), # Tells R the year is at the end  
    period = case_when(
      Date >= "2023-07-01" & Date <= "2023-12-31" ~ 1,
      Date >= "2024-01-01" & Date <= "2024-06-30" ~ 2,
      Date >= "2024-07-01" & Date <= "2024-12-31" ~ 3,
      Date >= "2025-01-01" & Date <= "2025-06-30" ~ 4,
      Date >= "2025-07-01" & Date <= "2025-12-31" ~ 5,
      Date >= "2026-01-01" & Date <= "2026-06-30" ~ 6,
      Date >= "2026-07-01" & Date <= "2026-12-31" ~ 7,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-c(Period, Rank, Date)) %>%
  rename(trainerfullname=Trainer) %>%
  relocate(Period=period, .before=RATING)

pre_model_trainer <- pre_model_data %>%
  mutate(Date=meetingdate,
         Period = case_when(
           Date >= "2023-07-01" & Date <= "2023-12-31" ~ 0,
           Date >= "2024-01-01" & Date <= "2024-06-30" ~ 1,
           Date >= "2024-07-01" & Date <= "2024-12-31" ~ 2,
           Date >= "2025-01-01" & Date <= "2025-06-30" ~ 3,
           Date >= "2025-07-01" & Date <= "2025-12-31" ~ 4,
           Date >= "2026-01-01" & Date <= "2026-06-30" ~ 5,
           Date >= "2025-01-01" & Date <= "2025-06-30" ~ 3,
           Date >= "2025-07-01" & Date <= "2025-12-31" ~ 4,
           Date >= "2026-01-01" & Date <= "2026-06-30" ~ 5,
           Date >= "2026-07-01" & Date <= "2026-12-31" ~ 6,
           TRUE ~ NA_real_
         )
  ) %>%
  left_join(., trainer_ratings_dat, by=c('trainerfullname', 'Period'))

missing_trainers <- pre_model_trainer %>%
  filter(is.na(Rating)) %>% 
  select(trainercode, trainerfullname, Rating) %>%
  unique()


#### pace analysis ####

# raceclass NA fill
pace_dat <- tbl(kon, "gb_racing_data") |>
  select(raceid, scheduledtimeofracegmt, countrycode, coursename, racenumber, 
         racetype, racesurfacename, distance, numberofrunners, draw,
         raceclass, racecode, pacemapdata,
         iphintsoverallpace,
         positionofficial, betfairwinsp
  ) %>%
  filter(countrycode == 'GBR',
         racetype == 'Flat') %>%
  distinct() %>%
  collect()

pace_df <- pace_dat %>%
  filter(!is.na(betfairwinsp)) %>%
  mutate(across(c(raceclass, racecode, contains('pace')), ~as.factor(.))) %>%
  mutate(pacemapdata = fct(str_extract(pacemapdata, '\\d+$')),
         win = ifelse(positionofficial==1, 1, 0),
         bsp = ifelse(betfairwinsp > 100, 100, betfairwinsp))

pace_dat_summ <- pace_df %>%
  filter(iphintsoverallpace != 'Insufficient Data',
         betfairwinsp < 20
         ) %>%
  group_by(pacemapdata, iphintsoverallpace) %>%
  summarise(runners = n(),
            strike_rate = mean(win),
            ave_price = mean(betfairwinsp)
            ) %>%
  ungroup() %>%
  filter(runners >= 30)
  


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