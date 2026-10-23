# ===== Step 10: Neuropathic pain pathway analyses (follows the neuropathic pain DAG) =====
# Poisson regression with robust standard errors -> prevalence ratios (PR).
# PR = how many times more likely to screen positive for neuropathic pain (DN4 >= 3 of 7).
#
# Neuropathic pain is adjusted for AGE (its main driver in our data) plus ONE more
# variable chosen from the DAG for each exposure:
#   Metabolic pathway (main):   obesity/BMI -> age + sex     (NOT diabetes: it is on the pathway)
#                               diabetes, hypertension -> age + obesity (obesity causes both)
#   Work/mechanical pathway:    age + Indigenous language (marker of marginalization)
#   Toxic pathway:              pesticides -> age + Indigenous language; alcohol -> age + sex
#
# N1  Screening table: every DAG predictor, crude vs DAG-adjusted PR, with p-values
# N2  Dose-response: metabolic score (0-3) and mechanical score (0-3)
# N3  Healthy worker check: is neuropathic pain pushing people out of hand-intensive work?

library(tidyverse)
library(sandwich)
options(width = 220)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

# --- 1. Prepare variables (1 = yes, 0 = no) ---------------------------------
an <- dat %>%
  mutate(
    neuropathic_yes = as.numeric(neuropathic == "Yes"),
    female          = as.numeric(sex == 0),
    age10           = age / 10,                     # PR per 10 years of age
    bmi5            = bmi / 5,                      # PR per 5 BMI points
    obese           = as.numeric(bmi >= 30),
    diabetes        = health_cond_diabetes,
    hypertension    = health_cond_hypertension,
    years_10plus    = as.numeric(occupation_years == 4),
    alcohol_30d     = case_when(alc_ever == 0 ~ 0, alc_30days >= 1 ~ 1, alc_30days == 0 ~ 0),

    # NEW SCORE 1: metabolic score (0-3) = obese + diabetes + hypertension
    metabolic_score  = obese + diabetes + hypertension,
    # NEW SCORE 2: mechanical score (0-3) = repetitive movements + heavy lifting + bending/twisting
    mechanical_score = repetitive_movements + heavy_lifting + bending_twisting,

    work_status = case_when(
      occupation == 0          ~ "Agricultural worker",
      occupation == 1          ~ "Domestic worker/homemaker",
      occupation %in% c(3, 4)  ~ "Unemployed",
      !is.na(occupation)       ~ "Other work")
  )

# --- 2. Helper: crude and adjusted PR for one exposure, on the SAME people ---
# Both models use only people with data on the exposure AND the adjustment variables,
# so any change between crude and adjusted comes from the adjustment, not a different sample.
# Yes/no exposures: skipped if either group has < 3 cases, flagged if 3-4 cases.
fit_one <- function(formula, term, d) {
  m  <- glm(formula, family = poisson(link = "log"), data = d)
  se <- sqrt(diag(vcovHC(m, type = "HC0")))[term]
  b  <- coef(m)[term]
  list(txt = sprintf("%.2f (%.2f-%.2f)", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se)),
       p = 2 * pnorm(-abs(b / se)))
}
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

screen <- function(pathway, label, term, adj, adj_label, data = an) {
  vars <- c("neuropathic_yes", term, adj)
  d <- data[complete.cases(data[, vars]), ]
  y <- d$neuropathic_yes; x <- d[[term]]
  note <- ""
  if (all(x %in% c(0, 1))) {
    fewest <- min(sum(y == 1 & x == 1), sum(y == 1 & x == 0))
    if (fewest < 3)
      return(tibble(Pathway = pathway, Variable = label, Answered = nrow(d), Cases = sum(y),
                    `Crude PR (95% CI)` = "", `p crude` = "", `Adjusted PR (95% CI)` = "",
                    `p adjusted` = "", `Adjusted for` = adj_label,
                    Note = sprintf("Skipped: only %d case(s) in one group", fewest)))
    if (fewest < 5) note <- sprintf("Only %d cases in one group; interpret with caution", fewest)
  }
  cr <- fit_one(reformulate(term, "neuropathic_yes"), term, d)
  ad <- fit_one(reformulate(c(term, adj), "neuropathic_yes"), term, d)
  tibble(Pathway = pathway, Variable = label, Answered = nrow(d), Cases = sum(y),
         `Crude PR (95% CI)` = cr$txt, `p crude` = fmt_p(cr$p),
         `Adjusted PR (95% CI)` = ad$txt, `p adjusted` = fmt_p(ad$p),
         `Adjusted for` = adj_label, Note = note)
}

# ============================================================================
# N1. Screening table: every predictor in the DAG
# ============================================================================
n1 <- bind_rows(
  # Background factors
  screen("Background", "Age, per 10 years",               "age10",  "female",              "Sex"),
  screen("Background", "Female",                          "female", "age10",               "Age"),
  screen("Background", "Speaks an Indigenous language",   "indig_lang", "age10",           "Age"),
  # Metabolic pathway (main)
  screen("Metabolic (main)", "BMI, per 5 points",         "bmi5",   c("age10", "female"),  "Age, sex"),
  screen("Metabolic (main)", "Obese (BMI 30 or more)",    "obese",  c("age10", "female"),  "Age, sex"),
  screen("Metabolic (main)", "Diabetes",                  "diabetes",     c("age10", "obese"), "Age, obesity"),
  screen("Metabolic (main)", "Hypertension",              "hypertension", c("age10", "obese"), "Age, obesity"),
  screen("Metabolic (main)", "Metabolic score, per condition (0-3)", "metabolic_score", c("age10", "female"), "Age, sex"),
  # Work / mechanical pathway (exploratory; reverse causation possible)
  screen("Work/mechanical", "Repetitive movements",       "repetitive_movements", c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Work/mechanical", "Heavy lifting",              "heavy_lifting",        c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Work/mechanical", "Bending/twisting",           "bending_twisting",     c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Work/mechanical", "Mechanical score, per exposure (0-3)", "mechanical_score", c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Work/mechanical", "Cold/wet conditions",        "cold_wet_conditions",  c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Work/mechanical", "More than 10 years in this work", "years_10plus",    c("age10", "indig_lang"), "Age, Indigenous language"),
  # Toxic pathway (exploratory)
  screen("Toxic", "Pesticides at work",                   "pesticide_exposure",   c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Toxic", "Pesticides at home",                   "pesticides_home",      c("age10", "indig_lang"), "Age, Indigenous language"),
  screen("Toxic", "Alcohol in past 30 days",              "alcohol_30d",          c("age10", "female"),     "Age, sex"),
  screen("Toxic", "Ever drank alcohol",                   "alc_ever",             c("age10", "female"),     "Age, sex")
)

# ============================================================================
# N2. Dose-response tables
# ============================================================================
prev_table <- function(var, label) {
  an %>%
    filter(!is.na(.data[[var]]), !is.na(neuropathic_yes)) %>%
    group_by(level = .data[[var]]) %>%
    summarise(people = n(), cases = sum(neuropathic_yes), .groups = "drop") %>%
    mutate(`% screened positive` = round(100 * cases / people, 1)) %>%
    mutate(Table = label, .before = 1)
}
n2 <- bind_rows(
  prev_table("metabolic_score",  "Neuropathic pain by metabolic score (obese + diabetes + hypertension)"),
  prev_table("mechanical_score", "Neuropathic pain by mechanical score (repetitive + lifting + bending)")
)

# ============================================================================
# N3. Healthy worker check
# ============================================================================
n3_table <- an %>%
  filter(!is.na(work_status), !is.na(neuropathic_yes)) %>%
  count(work_status, group = ifelse(neuropathic_yes == 1, "Neuropathic pain", "No neuropathic pain")) %>%
  group_by(group) %>%
  mutate(`% of group` = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(group, work_status)
n3_model <- screen("Healthy worker check", "Repetitive movements (agricultural workers only)",
                   "repetitive_movements", "age10", "Age",
                   data = filter(an, work_status == "Agricultural worker"))

# ============================================================================
# PRINT AND SAVE
# ============================================================================
cat("\n=================== N1: DAG PREDICTORS (crude vs adjusted) ===================\n")
print(as.data.frame(n1), right = FALSE, row.names = FALSE)

cat("\n=================== N2: DOSE-RESPONSE ===================\n")
print(as.data.frame(n2), right = FALSE, row.names = FALSE)

cat("\n=================== N3: HEALTHY WORKER CHECK ===================\n")
print(as.data.frame(n3_table), right = FALSE, row.names = FALSE)
print(as.data.frame(n3_model), right = FALSE, row.names = FALSE)

write_csv(bind_rows(n1, n3_model), "/Users/feliciasong/Desktop/neuropathic_pathway_models.csv")
write_csv(n2,       "/Users/feliciasong/Desktop/neuropathic_dose_response.csv")
write_csv(n3_table, "/Users/feliciasong/Desktop/neuropathic_work_status.csv")
