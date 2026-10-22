# ===== Step 9: Pathway analyses (follows the DAG) =====
# Poisson regression with robust standard errors -> prevalence ratios (PR).
# PR = how many times more likely to screen positive. PR 2.0 = twice as likely.
#
# PART A  Migraine (adjusted for SEX, plus one more variable chosen from the DAG)
#   A1  Does speaking an Indigenous language -> migraine shrink once work exposures are added?
#   A2  Does migraine rise with MORE physical/heat exposure? (dose-response)
#   A3  Main migraine models
# PART B  Neuropathic pain (adjusted for AGE, plus one more variable chosen from the DAG)
#   B1  Main neuropathic pain models (metabolic pathway)
#   B2  Is neuropathic pain pushing people out of work? (healthy worker check)

library(tidyverse)
library(sandwich)
options(width = 200)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

# --- 1. Prepare variables (1 = yes, 0 = no) ---------------------------------
an <- dat %>%
  mutate(
    migraine_yes    = as.numeric(migraine == "Yes"),
    neuropathic_yes = as.numeric(neuropathic == "Yes"),
    female          = as.numeric(sex == 0),
    age10           = age / 10,                 # PR per 10 years of age
    bmi5            = bmi / 5,                  # PR per 5 BMI points
    obese           = as.numeric(bmi >= 30),
    diabetes        = health_cond_diabetes,
    hypertension    = health_cond_hypertension,

    # NEW SCORE 1: exertion + heat (0, 1, or 2)
    #   0 = neither heavy lifting nor hot conditions, 1 = one of them, 2 = both
    exertion_heat   = heavy_lifting + heat_conditions,

    # NEW SCORE 2: total physical workload (number of the 6 physical demands reported, 0-6)
    workload        = heavy_lifting + standing_long_hours + bending_twisting +
                      repetitive_movements + heat_conditions + cold_wet_conditions,

    # Current work situation (for the healthy worker check)
    work_status = case_when(
      occupation == 0          ~ "Agricultural worker",
      occupation == 1          ~ "Domestic worker/homemaker",
      occupation %in% c(3, 4)  ~ "Unemployed",
      !is.na(occupation)       ~ "Other work")
  )

# --- 2. Helper: fit one model, return the PR for one variable ----------------
# For yes/no variables, the model is skipped if either group has fewer than 3 cases
# (too few to estimate), and flagged if either group has 3-4 cases.
pr <- function(formula, term, data = an, section, question, model) {
  vars <- all.vars(formula)
  d    <- data[complete.cases(data[, vars]), ]
  y    <- d[[vars[1]]]; x <- d[[term]]
  note <- ""
  if (all(x %in% c(0, 1))) {
    fewest <- min(sum(y == 1 & x == 1), sum(y == 1 & x == 0))
    if (fewest < 3)
      return(tibble(Section = section, Question = question, Model = model, Variable = term,
                    n = nrow(d), cases = sum(y), `PR (95% CI)` = "", p = "",
                    Note = sprintf("Skipped: only %d case(s) in one group", fewest), PR_num = NA_real_))
    if (fewest < 5) note <- sprintf("Only %d cases in one group; interpret with caution", fewest)
  }
  m  <- glm(formula, family = poisson(link = "log"), data = d)
  se <- sqrt(diag(vcovHC(m, type = "HC0")))[term]
  b  <- coef(m)[term]
  p  <- 2 * pnorm(-abs(b / se))
  tibble(Section = section, Question = question, Model = model,
         Variable = term,
         n = nobs(m), cases = sum(m$y),
         `PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se)),
         p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
         Note = note, PR_num = exp(b))
}

# Prevalence table: how many screened positive at each level of a variable
prev_table <- function(data, outcome, var, label) {
  data %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[outcome]])) %>%
    group_by(level = .data[[var]]) %>%
    summarise(people = n(), cases = sum(.data[[outcome]]), .groups = "drop") %>%
    mutate(`% screened positive` = round(100 * cases / people, 1)) %>%
    mutate(Table = label, .before = 1)
}

# ============================================================================
# PART A  MIGRAINE
# ============================================================================

# --- A1. Indigenous language -> migraine: explained by work? ---------------
# Step 1 uses everyone (136). Steps 2 and 3 use ONLY the 111 people who answered the
# work questions, so the comparison between Step 2 and Step 3 is on the same people.
# If the PR for Indigenous language gets closer to 1.00 in Step 3, part of the link
# runs through harder/hotter work.
work_sample <- an %>% filter(!is.na(exertion_heat), !is.na(indig_lang), !is.na(migraine_yes))

a1 <- bind_rows(
  pr(migraine_yes ~ indig_lang + female, "indig_lang", an,
     "A1", "Indigenous language -> migraine", "Step 1: + sex (everyone)"),
  pr(migraine_yes ~ indig_lang + female, "indig_lang", work_sample,
     "A1", "Indigenous language -> migraine", "Step 2: + sex (work-question sample)"),
  pr(migraine_yes ~ indig_lang + female + exertion_heat, "indig_lang", work_sample,
     "A1", "Indigenous language -> migraine", "Step 3: + sex + exertion/heat score (same sample)")
)
pr2 <- a1$PR_num[2]; pr3 <- a1$PR_num[3]
a1_explained <- if (!is.na(pr2) && !is.na(pr3) && pr2 > 1) round(100 * (pr2 - pr3) / (pr2 - 1)) else NA

# --- A2. Dose-response: more exposure -> more migraine? ----------------------
a2_tables <- bind_rows(
  prev_table(an, "migraine_yes", "exertion_heat", "Migraine by exertion/heat score (0-2)"),
  prev_table(an, "migraine_yes", "workload",      "Migraine by number of physical demands (0-6)")
)
a2 <- bind_rows(
  pr(migraine_yes ~ exertion_heat + female, "exertion_heat", an,
     "A2", "More exertion/heat -> more migraine?", "Per additional exposure (0-2) + sex"),
  pr(migraine_yes ~ workload + female, "workload", an,
     "A2", "More physical demands -> more migraine?", "Per additional demand (0-6) + sex")
)

# --- A3. Main migraine models (sex + one variable from the DAG) --------------
a3 <- bind_rows(
  pr(migraine_yes ~ heavy_lifting + female + indig_lang, "heavy_lifting", an,
     "A3", "Heavy lifting -> migraine", "+ sex + Indigenous language"),
  pr(migraine_yes ~ heat_conditions + female + indig_lang, "heat_conditions", an,
     "A3", "Hot conditions -> migraine", "+ sex + Indigenous language"),
  pr(migraine_yes ~ exertion_heat + female + indig_lang, "exertion_heat", an,
     "A3", "Exertion/heat score -> migraine", "+ sex + Indigenous language"),
  pr(migraine_yes ~ hypertension + female + age10, "hypertension", an,
     "A3", "Hypertension -> migraine", "+ sex + age (age drives hypertension)")
)

# ============================================================================
# PART B  NEUROPATHIC PAIN
# ============================================================================

# --- B1. Metabolic pathway (age + one variable from the DAG) -----------------
# Obesity/BMI: adjust for age + sex. Do NOT add diabetes (it is on the pathway).
# Diabetes: adjust for age + obesity (obesity causes both diabetes and neuropathy).
b1 <- bind_rows(
  pr(neuropathic_yes ~ bmi5 + age10 + female, "bmi5", an,
     "B1", "BMI -> neuropathic pain", "Per 5 BMI points + age + sex"),
  pr(neuropathic_yes ~ obese + age10 + female, "obese", an,
     "B1", "Obesity -> neuropathic pain", "+ age + sex"),
  pr(neuropathic_yes ~ diabetes + age10 + obese, "diabetes", an,
     "B1", "Diabetes -> neuropathic pain", "+ age + obesity")
)

# --- B2. Healthy worker check -------------------------------------------------
# (a) Are people with neuropathic pain less often working now?
b2_table <- an %>%
  filter(!is.na(work_status), !is.na(neuropathic_yes)) %>%
  count(work_status, neuropathic = ifelse(neuropathic_yes == 1, "Neuropathic pain", "No neuropathic pain")) %>%
  group_by(neuropathic) %>%
  mutate(`% of group` = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(neuropathic, work_status)
# (b) Repetitive movements among CURRENT agricultural workers only
ag_only <- an %>% filter(work_status == "Agricultural worker")
b2 <- pr(neuropathic_yes ~ repetitive_movements + age10, "repetitive_movements", ag_only,
         "B2", "Repetitive movements -> neuropathic pain", "Agricultural workers only, + age")

# ============================================================================
# PRINT AND SAVE
# ============================================================================
results <- bind_rows(a1, a2, a3, b1, b2) %>% select(-PR_num)

cat("\n=================== MODEL RESULTS ===================\n")
print(as.data.frame(results), right = FALSE, row.names = FALSE)

cat("\nA1: Share of the Indigenous language -> migraine link explained by exertion/heat:",
    ifelse(is.na(a1_explained), "not calculable", paste0(a1_explained, "%")), "\n")

cat("\n=================== A2: DOSE-RESPONSE TABLES ===================\n")
print(as.data.frame(a2_tables), right = FALSE, row.names = FALSE)

cat("\n=================== B2: WORK STATUS BY NEUROPATHIC PAIN ===================\n")
print(as.data.frame(b2_table), right = FALSE, row.names = FALSE)

write_csv(results,  "/Users/feliciasong/Desktop/pathway_models.csv")
write_csv(a2_tables, "/Users/feliciasong/Desktop/pathway_dose_response.csv")
write_csv(b2_table,  "/Users/feliciasong/Desktop/pathway_work_status.csv")
