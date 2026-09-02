library(dplyr)

# raw_data: one row per horse per race, with columns
# racecode, race_type, raceclass, age_category_group, sex_category_group,
# performancerating, timefigure, postrace_rating_raw

# ---- 1. Aggregate to the same group level as your summary file ----
group_summary <- raw_data %>%
  group_by(racecode, race_type, raceclass, age_category_group, sex_category_group) %>%
  summarise(
    n_races                 = n(),
    avg_performancerating   = mean(performancerating,   na.rm = TRUE),
    avg_timefigure          = mean(timefigure,          na.rm = TRUE),
    avg_postrace_rating_raw = mean(postrace_rating_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_races >= 12,
         !is.na(race_type),
         !is.na(raceclass))

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
group_summary <- group_summary %>%
  mutate(
    blended_score = own_data_weight * own_raw_score + (1 - own_data_weight) * prior_score
  ) %>%
  arrange(desc(blended_score)) %>%
  mutate(rank = row_number())


