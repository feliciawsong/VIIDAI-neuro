# ===== Data check list: IDs to look up on the paper forms =====

library(tidyverse)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

flag <- function(rows, issue, value) {
  rows %>% transmute(subid, issue = issue, value = as.character({{ value }}))
}

checks <- bind_rows(
  flag(filter(dat, time_shower > 24),
       "Hours until shower > 24", time_shower),
  flag(filter(dat, pesticide_training == 1, pesticide_training_time == 0),
       "Had pesticide training but 0 minutes", pesticide_training_time),
  flag(filter(dat, bmi > 45 | bmi < 18.5),
       "BMI extreme (check height/weight)", sprintf("BMI %.1f (wt %s, ht %s)", bmi, weight, height)),
  flag(filter(dat, packs_per_day >= 3 | packs_per_day == 0),
       "Packs per day 0 or 3+", packs_per_day),
  flag(filter(dat, years_tobacco >= 40),
       "Years smoking 40+", sprintf("%s years (age %s)", years_tobacco, age)),
  flag(filter(dat, tobacco_ever %in% c(1, 2), is.na(years_tobacco)),
       "Smoker but years smoking missing", tobacco_ever),
  flag(filter(dat, length_days == 0),
       "Days difficulty lasted = 0", sprintf("difficulty_7days = %s", difficulty_7days)),
  flag(filter(dat, difficulty_7days == 0, !is.na(length_days)),
       "No difficulty but days answered", length_days),
  flag(filter(dat, !is.na(occupation_other), occupation != 99),
       "Occupation write-in but not coded Other (99)",
       sprintf("code %s: %s", occupation, occupation_other)),
  flag(filter(dat, ethnicity == 99),
       "Ethnicity Other (99)", ethnicity_other)
) %>% arrange(issue, subid)

print(checks, n = Inf, width = Inf)

cat("\nPhysical activity (raw codes) — only 58 answered and no one is 'Light' (1):\n")
print(table(dat$physical_activity, useNA = "ifany"))

write_csv(checks, "/Users/feliciasong/Desktop/data_checks.csv")
