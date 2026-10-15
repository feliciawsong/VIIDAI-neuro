# ===== Step 6: Bivariate analysis — migraine and neuropathic pain =====

library(tidyverse)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

# --- 1. Prepare variables (1 = yes, 0 = no) ---
an <- dat %>%
  mutate(
    female          = as.numeric(sex == 0),
    origin          = factor(origin_grp, levels = c("Baja California", "Oaxaca", "Other")),
    occupation3     = factor(occ_grp, levels = c("Agricultural worker",
                                                 "Domestic worker/homemaker", "Other")),
    alcohol_30d     = case_when(alc_ever == 0 ~ 0, alc_30days >= 1 ~ 1, alc_30days == 0 ~ 0),
    tobacco_any     = case_when(tobacco_ever %in% c(1, 2) ~ 1, tobacco_ever == 0 ~ 0),
    hours_50plus    = as.numeric(occupation_hours == 4),
    years_10plus    = as.numeric(occupation_years == 4),
    migraine_yes    = as.numeric(migraine == "Yes"),
    neuropathic_yes = as.numeric(neuropathic == "Yes")
  )

# --- 2. The 20 variables (+ each outcome against the other) ---
vars <- tribble(
  ~var,                    ~label,                                ~type,
  "female",                "Female",                              "bin",
  "age",                   "Age, years",                          "cont",
  "origin",                "State of origin",                     "cat",
  "indig_lang",            "Speaks an Indigenous language",       "bin",
  "occupation3",           "Occupation",                          "cat",
  "can_read",              "Can read and write",                  "bin",
  "health_cond_diabetes",  "Diabetes",                            "bin",
  "health_cond_hypertension", "Hypertension",                     "bin",
  "bmi",                   "BMI",                                 "cont",
  "alcohol_30d",           "Alcohol in past 30 days",             "bin",
  "tobacco_any",           "Ever used tobacco",                   "bin",
  "hours_50plus",          "Works 50+ hours/week",                "bin",
  "years_10plus",          "More than 10 years in this work",     "bin",
  "heavy_lifting",         "Heavy lifting",                       "bin",
  "repetitive_movements",  "Repetitive movements",                "bin",
  "bending_twisting",      "Bending/twisting",                    "bin",
  "heat_conditions",       "Hot conditions",                      "bin",
  "cold_wet_conditions",   "Cold/wet conditions",                 "bin",
  "pesticide_exposure",    "Pesticides at work",                  "bin",
  "pesticides_home",       "Pesticides at home",                  "bin",
  "neuropathic_yes",       "Neuropathic pain (DN4 positive)",     "bin",
  "migraine_yes",          "Migraine (ID Migraine positive)",     "bin"
)

# --- 3. Compare one outcome across all variables ---
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

fisher_p <- function(tab) {
  if (min(dim(tab)) < 2) return(NA)
  tryCatch(fisher.test(tab)$p.value,
           error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value)
}

compare <- function(outcome) {
  pmap_dfr(vars, function(var, label, type) {
    if (var == outcome) return(tibble(Characteristic = label, n = "", No = "-", Yes = "-", p = ""))
    keep <- !is.na(an[[var]]) & !is.na(an[[outcome]])
    x <- an[[var]][keep]
    y <- an[[outcome]][keep]
    n <- as.character(sum(keep))

    if (type == "cont") {
      p  <- suppressWarnings(wilcox.test(x ~ y)$p.value)
      ms <- function(v) sprintf("%.1f (%.1f)", mean(v), sd(v))
      return(tibble(Characteristic = paste0(label, ", mean (SD)"), n = n,
                    No = ms(x[y == 0]), Yes = ms(x[y == 1]), p = fmt_p(p)))
    }

    p  <- fisher_p(table(x, y))
    cp <- function(k, g) {
      a <- sum(x == k & y == g)
      sprintf("%d (%.1f)", a, 100 * a / sum(y == g))
    }
    if (type == "bin")
      return(tibble(Characteristic = label, n = n, No = cp(1, 0), Yes = cp(1, 1), p = fmt_p(p)))

    lev <- levels(an[[var]])
    bind_rows(
      tibble(Characteristic = label, n = n, No = "", Yes = "", p = fmt_p(p)),
      tibble(Characteristic = paste0("   ", lev), n = "",
             No = map_chr(lev, cp, g = 0), Yes = map_chr(lev, cp, g = 1), p = "")
    )
  })
}

mig <- compare("migraine_yes")
neu <- compare("neuropathic_yes")

# --- 4. Combine side by side ---
nm <- table(an$migraine_yes)
nn <- table(an$neuropathic_yes)
biv_table <- tibble(
  Characteristic = mig$Characteristic,
  n = mig$n,
  !!paste0("Migraine No (n=", nm["0"], ")")     := mig$No,
  !!paste0("Migraine Yes (n=", nm["1"], ")")    := mig$Yes,
  `p (migraine)` = mig$p,
  !!paste0("Neuropathic No (n=", nn["0"], ")")  := neu$No,
  !!paste0("Neuropathic Yes (n=", nn["1"], ")") := neu$Yes,
  `p (neuropathic)` = neu$p
)
print(biv_table, n = Inf, width = Inf)

# --- 5. Which variables move forward (p < 0.20)? ---
cat("\nMIGRAINE, p < 0.20:\n")
mig %>% filter(p != "", p == "<0.001" | suppressWarnings(as.numeric(p)) < 0.20) %>%
  select(Characteristic, p) %>% print()
cat("\nNEUROPATHIC PAIN, p < 0.20:\n")
neu %>% filter(p != "", p == "<0.001" | suppressWarnings(as.numeric(p)) < 0.20) %>%
  select(Characteristic, p) %>% print()

# --- 6. Save for Excel ---
write_csv(biv_table, "/Users/feliciasong/Desktop/table_bivariate.csv", na = "")
