# ===== Step 6b: Bivariate analysis, round 2 (next 20 variables) =====

library(tidyverse)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

# --- 1. Prepare variables (1 = yes, 0 = no) ---
an <- dat %>%
  mutate(
    indigenous_eth   = case_when(ethnicity == 1 ~ 1, ethnicity %in% c(0, 2) ~ 0),
    obese            = as.numeric(bmi >= 30),
    age_40plus       = as.numeric(age >= 40),
    children_n       = if_else(have_children == 0, 0, children_num),
    hours_40plus     = as.numeric(occupation_hours >= 3),
    pest_worry_any   = as.numeric(pest_health_worry >= 1),
    ppe_always       = as.numeric(pesticide_protection == 2),
    harvest_or_cut   = case_when(activities_field_harvest == 1 | activities_field_cut == 1 ~ 1,
                                 activities_field_harvest == 0 & activities_field_cut == 0 ~ 0),
    any_activity     = as.numeric(physical_activity >= 1),
    migraine_yes     = as.numeric(migraine == "Yes"),
    neuropathic_yes  = as.numeric(neuropathic == "Yes")
  )

# --- 2. The next 20 variables ---
vars <- tribble(
  ~var,                     ~label,                                       ~type,
  "indigenous_eth",         "Identifies as Indigenous",                   "bin",
  "obese",                  "Obese (BMI 30 or more)",                     "bin",
  "age_40plus",             "Age 40 or older",                            "bin",
  "years_sq",               "Years in San Quintin",                       "cont",
  "hh_num",                 "Household size",                             "cont",
  "children_n",             "Number of children",                         "cont",
  "standing_long_hours",    "Standing for long periods",                  "bin",
  "primary_work_setting",   "Works mostly outdoors",                      "bin",
  "hours_40plus",           "Works 40+ hours/week",                       "bin",
  "health_cond_arthritis",  "Arthritis",                                  "bin",
  "alc_ever",               "Ever drank alcohol",                         "bin",
  "pest_clothes_worry",     "Worried about pesticides on clothes",        "bin",
  "pest_worry_any",         "Worried about pesticide health effects*",    "bin",
  "pest_symp_headaches",    "Headache after pesticide exposure*",         "bin",
  "pesticide_training",     "Received pesticide training*",               "bin",
  "ppe_always",             "Always uses protective equipment*",          "bin",
  "ag_product_strawberries","Works with strawberries*",                   "bin",
  "harvest_or_cut",         "Harvests or cuts*",                          "bin",
  "time_shower",            "Hours until shower after exposure*",         "cont",
  "any_activity",           "Any physical activity outside work (check coding)", "bin"
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
    tibble(Characteristic = label, n = n, No = cp(1, 0), Yes = cp(1, 1), p = fmt_p(p))
  })
}

mig <- compare("migraine_yes")
neu <- compare("neuropathic_yes")

# --- 4. Combine side by side ---
biv_table2 <- tibble(
  Characteristic = mig$Characteristic,
  `n (migraine)` = mig$n,
  `Migraine No` = mig$No, `Migraine Yes` = mig$Yes, `p (migraine)` = mig$p,
  `n (neuropathic)` = neu$n,
  `Neuropathic No` = neu$No, `Neuropathic Yes` = neu$Yes, `p (neuropathic)` = neu$p
)
print(biv_table2, n = Inf, width = Inf)

# --- 5. Which variables move forward (p < 0.20)? ---
cat("\nMIGRAINE, p < 0.20:\n")
mig %>% filter(p != "", p == "<0.001" | suppressWarnings(as.numeric(p)) < 0.20) %>%
  select(Characteristic, n, p) %>% print()
cat("\nNEUROPATHIC PAIN, p < 0.20:\n")
neu %>% filter(p != "", p == "<0.001" | suppressWarnings(as.numeric(p)) < 0.20) %>%
  select(Characteristic, n, p) %>% print()

# --- 6. Save for Excel ---
write_csv(biv_table2, "/Users/feliciasong/Desktop/table_bivariate_round2.csv", na = "")
