# ===== Table 8: Crude and age- and sex-adjusted prevalence ratios (exploratory) =====
# Poisson regression with robust standard errors, one model per variable.
# One-time install if needed: install.packages("sandwich")

library(tidyverse)
library(sandwich)
options(width = 200)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

# --- 1. Prepare variables (1 = yes, 0 = no) ---
an <- dat %>%
  mutate(
    migraine_yes    = as.numeric(migraine == "Yes"),
    neuropathic_yes = as.numeric(neuropathic == "Yes"),
    female          = as.numeric(sex == 0),
    age10           = age / 10,
    age_40plus      = as.numeric(age >= 40),
    oaxaca          = as.numeric(state_origin == 9),
    indigenous_eth  = case_when(ethnicity == 1 ~ 1, ethnicity %in% c(0, 2) ~ 0),
    ag_worker       = as.numeric(occupation == 0),
    years_sq10      = years_sq / 10,
    children_n      = if_else(have_children == 0, 0, children_num),
    bmi5            = bmi / 5,
    obese           = as.numeric(bmi >= 30),
    alcohol_30d     = case_when(alc_ever == 0 ~ 0, alc_30days >= 1 ~ 1, alc_30days == 0 ~ 0),
    tobacco_any     = case_when(tobacco_ever %in% c(1, 2) ~ 1, tobacco_ever == 0 ~ 0),
    hours_40plus    = as.numeric(occupation_hours >= 3),
    hours_50plus    = as.numeric(occupation_hours == 4),
    years_10plus    = as.numeric(occupation_years == 4)
  )

# --- 2. Variables from Table 6 (farmworker-only items and physical activity left out) ---
vars <- tribble(
  ~var,                       ~label,                                  ~type,
  "female",                   "Female",                                "bin",
  "age10",                    "Age, per 10 years",                     "cont",
  "age_40plus",               "Age 40 or older",                       "bin",
  "oaxaca",                   "From Oaxaca (vs other states)",         "bin",
  "indigenous_eth",           "Identifies as Indigenous",              "bin",
  "indig_lang",               "Speaks an Indigenous language",         "bin",
  "can_read",                 "Can read and write",                    "bin",
  "years_sq10",               "Years in San Quintin, per 10 years",    "cont",
  "hh_num",                   "Household size, per person",            "cont",
  "children_n",               "Number of children, per child",         "cont",
  "ag_worker",                "Agricultural worker (vs other jobs)",   "bin",
  "health_cond_diabetes",     "Diabetes",                              "bin",
  "health_cond_hypertension", "Hypertension",                          "bin",
  "health_cond_arthritis",    "Arthritis",                             "bin",
  "bmi5",                     "BMI, per 5 units",                      "cont",
  "obese",                    "Obese (BMI 30 or more)",                "bin",
  "alc_ever",                 "Ever drank alcohol",                    "bin",
  "alcohol_30d",              "Alcohol in past 30 days",               "bin",
  "tobacco_any",              "Ever used tobacco",                     "bin",
  "hours_40plus",             "Works 40+ hours/week",                  "bin",
  "hours_50plus",             "Works 50+ hours/week",                  "bin",
  "years_10plus",             "More than 10 years in this work",       "bin",
  "primary_work_setting",     "Works mostly outdoors",                 "bin",
  "heavy_lifting",            "Heavy lifting",                         "bin",
  "repetitive_movements",     "Repetitive movements",                  "bin",
  "bending_twisting",         "Bending/twisting",                      "bin",
  "standing_long_hours",      "Standing for long periods",             "bin",
  "heat_conditions",          "Hot conditions",                        "bin",
  "cold_wet_conditions",      "Cold/wet conditions",                   "bin",
  "pesticide_exposure",       "Pesticides at work",                    "bin",
  "pesticides_home",          "Pesticides at home",                    "bin",
  "pest_clothes_worry",       "Worried about pesticides on clothes",   "bin",
  "neuropathic_yes",          "Neuropathic pain (DN4 positive)",       "bin",
  "migraine_yes",             "Migraine (ID Migraine positive)",       "bin"
)

# --- 3. Helpers ---
fit_pr <- function(formula, term) {
  m  <- tryCatch(glm(formula, family = poisson(link = "log"), data = an), error = function(e) NULL)
  if (is.null(m) || !term %in% names(coef(m))) return(tibble(PR = NA, low = NA, high = NA, p = NA, n = NA, cases = NA))
  se <- sqrt(diag(vcovHC(m, type = "HC0")))[term]
  b  <- coef(m)[term]
  tibble(PR = exp(b), low = exp(b - 1.96 * se), high = exp(b + 1.96 * se),
         p = 2 * pnorm(-abs(b / se)), n = nobs(m), cases = sum(m$y))
}
fmt_pr <- function(d) ifelse(is.na(d$PR), "", sprintf("%.2f (%.2f-%.2f)", d$PR, d$low, d$high))
fmt_p  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
MIN_CASES  <- 3   # skip a binary variable if either group has fewer than 3 cases (model can't estimate reliably)
FLAG_CASES <- 5   # flag (but keep) if either group has 3-4 cases: interpret with caution

run_outcome <- function(outcome, outcome_label) {
  pmap_dfr(vars, function(var, label, type) {
    if (var == outcome) return(NULL)
    keep <- !is.na(an[[var]]) & !is.na(an[[outcome]])
    y <- an[[outcome]][keep]; x <- an[[var]][keep]

    # too few cases? (binary variables: cases with vs without the characteristic)
    note <- ""
    if (type == "bin") {
      fewest <- min(sum(y == 1 & x == 1), sum(y == 1 & x == 0))
      if (fewest < MIN_CASES && var != "female")
        return(tibble(Outcome = outcome_label, Variable = label,
                      `n (cases)` = sprintf("%d (%d)", sum(keep), sum(y)),
                      `Crude PR (95% CI)` = "", `p (crude)` = "",
                      `Adjusted PR (95% CI)` = "", `p (adjusted)` = "",
                      `Adjusted for` = "", Note = sprintf("Skipped: only %d case(s) in one group", fewest)))
      if (fewest < FLAG_CASES) note <- sprintf("Only %d cases in one group; interpret with caution", fewest)
    }

    # adjustment set: age + sex, except for the age and sex variables themselves
    adj <- case_when(var == "female" ~ "age10",
                     var %in% c("age10", "age_40plus") ~ "female",
                     TRUE ~ "age10 + female")
    crude <- fit_pr(as.formula(paste(outcome, "~", var)), var)
    adjd  <- fit_pr(as.formula(paste(outcome, "~", var, "+", adj)), var)

    tibble(Outcome = outcome_label, Variable = label,
           `n (cases)` = sprintf("%d (%d)", crude$n, crude$cases),
           `Crude PR (95% CI)` = fmt_pr(crude), `p (crude)` = fmt_p(crude$p),
           `Adjusted PR (95% CI)` = fmt_pr(adjd), `p (adjusted)` = fmt_p(adjd$p),
           `Adjusted for` = recode(adj, age10 = "Age", female = "Sex", `age10 + female` = "Age, sex"),
           Note = note)
  })
}

table8 <- bind_rows(run_outcome("migraine_yes", "Migraine"),
                    run_outcome("neuropathic_yes", "Neuropathic pain"))

print(as.data.frame(table8), right = FALSE, row.names = FALSE)

write_csv(table8, "/Users/feliciasong/Desktop/table8_adjusted.csv")
