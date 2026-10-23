# ===== Step 11: Counts behind each model (who is in each regression) =====
# For every model in the migraine and neuropathic pain tables, this shows:
#   Answered = people in the model (complete data on the exposure AND its adjustment variables)
#   Cases    = of those, how many screened positive
#   With     = for yes/no exposures: cases / people WITH the exposure (e.g., hypertension = yes)
#   Without  = cases / people WITHOUT the exposure
# Crude and adjusted models use the same people, so there is one set of counts per row.
# Continuous variables and scores (age, BMI, 0-3 scores) have no with/without split;
# their counts by level are in the dose-response tables.

library(tidyverse)
options(width = 220)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

an <- dat %>%
  mutate(
    migraine_yes     = as.numeric(migraine == "Yes"),
    neuropathic_yes  = as.numeric(neuropathic == "Yes"),
    female           = as.numeric(sex == 0),
    age10            = age / 10,
    bmi5             = bmi / 5,
    obese            = as.numeric(bmi >= 30),
    diabetes         = health_cond_diabetes,
    hypertension     = health_cond_hypertension,
    years_10plus     = as.numeric(occupation_years == 4),
    alcohol_30d      = case_when(alc_ever == 0 ~ 0, alc_30days >= 1 ~ 1, alc_30days == 0 ~ 0),
    exertion_heat    = heavy_lifting + heat_conditions,
    metabolic_score  = obese + diabetes + hypertension,
    mechanical_score = repetitive_movements + heavy_lifting + bending_twisting,
    ag_worker        = as.numeric(occupation == 0)
  )

counts <- function(table, label, outcome, term, adj, data = an) {
  vars <- c(outcome, term, adj)
  d <- data[complete.cases(data[, vars]), ]
  y <- d[[outcome]]; x <- d[[term]]
  yes_no <- all(x %in% c(0, 1))
  tibble(Table = table, Variable = label, `Adjusted for` = adj_label(adj),
         Answered = nrow(d), Cases = sum(y),
         `With: cases/total`    = if (yes_no) sprintf("%d/%d", sum(y[x == 1]), sum(x == 1)) else "",
         `With: %`              = if (yes_no) sprintf("%.1f", 100 * sum(y[x == 1]) / sum(x == 1)) else "",
         `Without: cases/total` = if (yes_no) sprintf("%d/%d", sum(y[x == 0]), sum(x == 0)) else "",
         `Without: %`           = if (yes_no) sprintf("%.1f", 100 * sum(y[x == 0]) / sum(x == 0)) else "")
}
adj_label <- function(adj) {
  nice <- c(female = "Sex", age10 = "Age", indig_lang = "Indigenous language",
            obese = "Obesity", exertion_heat = "Exertion/heat score")
  paste(nice[adj], collapse = ", ")
}

# ---- Migraine pathway (Indigenous language steps) ----
mig_path <- bind_rows(
  counts("Migraine pathway", "Indigenous language (Step 1, everyone)",       "migraine_yes", "indig_lang", "female"),
  counts("Migraine pathway", "Indigenous language (Steps 2-3, work sample)", "migraine_yes", "indig_lang", c("female", "exertion_heat"))
)

# ---- Migraine models ----
mig_models <- bind_rows(
  counts("Migraine models", "Heavy lifting",                   "migraine_yes", "heavy_lifting",   c("female", "indig_lang")),
  counts("Migraine models", "Hot working conditions",          "migraine_yes", "heat_conditions", c("female", "indig_lang")),
  counts("Migraine models", "Exertion/heat score (0-2)",       "migraine_yes", "exertion_heat",   c("female", "indig_lang")),
  counts("Migraine models", "Hypertension",                    "migraine_yes", "hypertension",    c("female", "age10"))
)

# ---- Neuropathic pain pathways ----
neuro <- bind_rows(
  counts("Neuro pathways", "Age, per 10 years",                    "neuropathic_yes", "age10",            "female"),
  counts("Neuro pathways", "Female",                               "neuropathic_yes", "female",           "age10"),
  counts("Neuro pathways", "Speaks an Indigenous language",        "neuropathic_yes", "indig_lang",       "age10"),
  counts("Neuro pathways", "BMI, per 5 points",                    "neuropathic_yes", "bmi5",             c("age10", "female")),
  counts("Neuro pathways", "Obese (BMI 30 or more)",               "neuropathic_yes", "obese",            c("age10", "female")),
  counts("Neuro pathways", "Diabetes",                             "neuropathic_yes", "diabetes",         c("age10", "obese")),
  counts("Neuro pathways", "Hypertension",                         "neuropathic_yes", "hypertension",     c("age10", "obese")),
  counts("Neuro pathways", "Metabolic score (0-3)",                "neuropathic_yes", "metabolic_score",  c("age10", "female")),
  counts("Neuro pathways", "Repetitive movements",                 "neuropathic_yes", "repetitive_movements", c("age10", "indig_lang")),
  counts("Neuro pathways", "Heavy lifting",                        "neuropathic_yes", "heavy_lifting",        c("age10", "indig_lang")),
  counts("Neuro pathways", "Bending/twisting",                     "neuropathic_yes", "bending_twisting",     c("age10", "indig_lang")),
  counts("Neuro pathways", "Mechanical score (0-3)",               "neuropathic_yes", "mechanical_score",     c("age10", "indig_lang")),
  counts("Neuro pathways", "Cold/wet conditions",                  "neuropathic_yes", "cold_wet_conditions",  c("age10", "indig_lang")),
  counts("Neuro pathways", "More than 10 years in this work",      "neuropathic_yes", "years_10plus",         c("age10", "indig_lang")),
  counts("Neuro pathways", "Pesticides at work",                   "neuropathic_yes", "pesticide_exposure",   c("age10", "indig_lang")),
  counts("Neuro pathways", "Pesticides at home",                   "neuropathic_yes", "pesticides_home",      c("age10", "indig_lang")),
  counts("Neuro pathways", "Alcohol in past 30 days",              "neuropathic_yes", "alcohol_30d",          c("age10", "female")),
  counts("Neuro pathways", "Ever drank alcohol",                   "neuropathic_yes", "alc_ever",             c("age10", "female")),
  counts("Neuro work check", "Repetitive movements (agricultural workers only)", "neuropathic_yes", "repetitive_movements", "age10",
         data = filter(an, ag_worker == 1))
)

all_counts <- bind_rows(mig_path, mig_models, neuro)
cat("\n=================== COUNTS BEHIND EACH MODEL ===================\n")
print(as.data.frame(all_counts), right = FALSE, row.names = FALSE)

write_csv(all_counts, "/Users/feliciasong/Desktop/model_counts.csv")
