library(dplyr)

# class_data: one row per horse per race, with columns
# racecode, race_type, raceclass, age_category_group, sex_category_group,
# performancerating, timefigure, postrace_rating_raw
class_data <- pre_model_data %>%
  mutate(age_category_group=case_when(
    horseage %in% c(1, 2) ~ '2yo only',
    horseage > 2 ~ 'Older (3yo+, ages combined)',
    T ~ NA_character_
  ),
  sex_category_group_1=case_when(
    horsegender == "f" ~ "Fillies only",
    horsegender %in% c("f", "m") ~ "Fillies & Mares",
    horsegender %in% c("c", "g", "h", "m", "r") ~ "Open",
    TRUE ~ NA_character_
  ),
  sex_category_group = case_when(
    sex_category_group_1 %in% c("Fillies & Mares", "Fillies & Mares (combined)") ~ "Fillies & Mares",
    sex_category_group_1 == "Fillies only" ~ "Fillies only",
    TRUE ~ "Open"  # includes Open, Colts-only, Colts & Geldings, Colts & Fillies, etc.
  ),
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
  )
    ) %>%
  select(raceid, horsecode, horseage, horsegender, racecode, race_type, raceclass, age_category_group, sex_category_group,
         performancerating, timefigure, postrace_rating_raw) %>%
  filter(performancerating > 0,
         !is.na(race_type),
         !is.na(raceclass))

# ---- 1. Aggregate to the same group level as your summary file ----
group_summary <- class_data %>%
  group_by(racecode, race_type, raceclass, age_category_group, sex_category_group) %>%
  summarise(
    n_races                 = n(),
    avg_performancerating   = mean(performancerating,   na.rm = TRUE),
    avg_timefigure          = mean(timefigure,          na.rm = TRUE),
    avg_postrace_rating_raw = mean(postrace_rating_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_races >= 12)

# ---- 2. own_raw_score: weighted average of standardized (z-scored) ratings ----
# weights ~0.35 / 0.22 / 0.33 fit the sample data with R^2 = 0.9999
group_summary <- group_summary %>%
  mutate(
    z_perf     = as.numeric(scale(avg_performancerating)),
    z_time     = as.numeric(scale(avg_timefigure)),
    z_postrace = as.numeric(scale(avg_postrace_rating_raw)),
    own_raw_score = 0.35 * z_perf + 0.22 * z_time + 0.33 * z_postrace + 0.45
  )

# ---- 3. own_data_weight: credibility weight from sample size ----
# fits own_data_weight = n / (n + 15) almost exactly (mean abs error ~0.003)
group_summary <- group_summary %>%
  mutate(
    own_data_weight = n_races / (n_races + 15),
    confidence = case_when(
      n_races >= 100            ~ "Very High (n>=100)",
      n_races >= 50             ~ "High (50-99)",
      n_races >= 20             ~ "Medium (20-49)",
      n_races >= 12             ~ "Low (12-19)",
      TRUE                      ~ "Very Low (n<12)"
    )
  )

# ---- 4. prior_score: class-level baseline to shrink toward ----
# simplest version = mean own_raw_score within raceclass; swap for a
# finer grouping (e.g. racecode) if you want the closer within-class fit
group_summary <- group_summary %>%
  group_by(raceclass) %>%
  mutate(prior_score = mean(own_raw_score, na.rm = TRUE)) %>%
  ungroup()

# ---- 5. blended_score + rank ----
group_summary_1 <- group_summary %>%
  mutate(
    blended_score = own_data_weight * own_raw_score + (1 - own_data_weight) * prior_score
  ) %>%
  arrange(desc(blended_score)) %>%
  mutate(rank = row_number()) %>%
  relocate(rank, .before=racecode) %>%
  select(racecode, race_type, raceclass, age_category_group, sex_category_group,
         category_confidence=confidence, category_rank=rank, 
         category_blended_score=blended_score)


group_summary_2 <- class_rankings %>%
  select(race_type, raceclass=raceclass_pooled, age_category_group, sex_category_group, starts_with('category_')) %>%
  distinct() %>%
  mutate(raceclass=as.factor(raceclass))


model_data_class <- pre_model_data  %>%
  mutate(age_category_group=case_when(
    horseage %in% c(1, 2) ~ '2yo only',
    horseage > 2 ~ 'Older (3yo+, ages combined)',
    T ~ NA_character_
    ),
    sex_category_group_1=case_when(
      horsegender == "f" ~ "Fillies only",
      horsegender %in% c("f", "m") ~ "Fillies & Mares",
      horsegender %in% c("c", "g", "h", "m", "r") ~ "Open",
      TRUE ~ NA_character_
    ),
    sex_category_group = case_when(
          sex_category_group_1 %in% c("Fillies & Mares", "Fillies & Mares (combined)") ~ "Fillies & Mares",
          sex_category_group_1 == "Fillies only" ~ "Fillies only",
          TRUE ~ "Open"  # includes Open, Colts-only, Colts & Geldings, Colts & Fillies, etc.
    ),
    race_type = case_when(
      racecode == "E"  ~ "Claimer",
      racecode == "G"  ~ "Maiden",
      racecode == "G1" ~ "Group 1",
      racecode == "G2" ~ "Group 2",
      racecode == "G3" ~ "Group 3",
      racecode == "GA" ~ "Maiden",
      racecode == "I"  ~ "Handicap",
      racecode == "IA" ~ "Handicap (Apprentice)",
      racecode == "IM" ~ "Handicap (Amateur)",
      racecode == "L"  ~ "Listed",
      racecode == "P"  ~ "Novice",
      racecode == "W"  ~ "Seller",
      racecode == "X"  ~ "Claiming Handicap",
      TRUE ~ NA_character_
    )
  ) %>%
  select(raceid, meetingdate, race_type, raceclass, age_category_group, sex_category_group) %>%
  distinct() %>%
  left_join(., group_summary_1, by=c('race_type', 'raceclass', 'age_category_group', 'sex_category_group')) %>%
  filter(!is.na(category_rank)) %>%
  group_by(raceid) %>%
  slice(1) %>% ungroup()


