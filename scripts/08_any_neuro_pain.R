# ===== Step 8: Any neurological pain (migraine OR neuropathic) =====
# Part A: bivariate (40 variables)   Part B: regression (prevalence ratios)
# One-time install if needed: install.packages("sandwich")

library(tidyverse)
library(sandwich)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

# --- 1. Prepare variables (1 = yes, 0 = no) ---
an <- dat %>%
  mutate(
    any_neuro       = as.numeric(migraine == "Yes" | neuropathic == "Yes"),
    female          = as.numeric(sex == 0),
    age10           = age / 10,
    age_40plus      = as.numeric(age >= 40),
    origin          = factor(origin_grp, levels = c("Baja California", "Oaxaca", "Other")),
    occupation3     = factor(occ_grp, levels = c("Agricultural worker",
                                                 "Domestic worker/homemaker", "Other")),
    indigenous_eth  = case_when(ethnicity == 1 ~ 1, ethnicity %in% c(0, 2) ~ 0),
    children_n      = if_else(have_children == 0, 0, children_num),
    obese           = as.numeric(bmi >= 30),
    alcohol_30d     = case_when(alc_ever == 0 ~ 0, alc_30days >= 1 ~ 1, alc_30days == 0 ~ 0),
    tobacco_any     = case_when(tobacco_ever %in% c(1, 2) ~ 1, tobacco_ever == 0 ~ 0),
    any_activity    = as.numeric(physical_activity >= 1),
    hours_40plus    = as.numeric(occupation_hours >= 3),
    hours_50plus    = as.numeric(occupation_hours == 4),
    years_10plus    = as.numeric(occupation_years == 4),
    pest_worry_any  = as.numeric(pest_health_worry >= 1),
    ppe_always      = as.numeric(pesticide_protection == 2),
    harvest_or_cut  = case_when(activities_field_harvest == 1 | activities_field_cut == 1 ~ 1,
                                activities_field_harvest == 0 & activities_field_cut == 0 ~ 0)
  )

cat("\nANY NEUROLOGICAL PAIN (migraine or neuropathic):\n")
k <- sum(an$any_neuro, na.rm = TRUE); n <- sum(!is.na(an$any_neuro))
ci <- binom.test(k, n)$conf.int
cat(sprintf("%d / %d = %.1f%% (95%% CI %.1f-%.1f)\n", k, n, 100 * k / n, 100 * ci[1], 100 * ci[2]))

# --- 2. The 40 variables, grouped ---
vars <- tribble(
  ~group,              ~var,                       ~label,                                  ~type,
  "Sociodemographic",  "female",                   "Female",                                "bin",
  "Sociodemographic",  "age",                      "Age, years",                            "cont",
  "Sociodemographic",  "age_40plus",               "Age 40 or older",                       "bin",
  "Sociodemographic",  "origin",                   "State of origin",                       "cat",
  "Sociodemographic",  "indigenous_eth",           "Identifies as Indigenous",              "bin",
  "Sociodemographic",  "indig_lang",               "Speaks an Indigenous language",         "bin",
  "Sociodemographic",  "can_read",                 "Can read and write",                    "bin",
  "Sociodemographic",  "years_sq",                 "Years in San Quintin",                  "cont",
  "Sociodemographic",  "hh_num",                   "Household size",                        "cont",
  "Sociodemographic",  "children_n",               "Number of children",                    "cont",
  "Sociodemographic",  "occupation3",              "Occupation",                            "cat",
  "Health",            "health_cond_diabetes",     "Diabetes",                              "bin",
  "Health",            "health_cond_hypertension", "Hypertension",                          "bin",
  "Health",            "health_cond_arthritis",    "Arthritis",                             "bin",
  "Health",            "bmi",                      "BMI",                                   "cont",
  "Health",            "obese",                    "Obese (BMI 30 or more)",                "bin",
  "Health",            "alc_ever",                 "Ever drank alcohol",                    "bin",
  "Health",            "alcohol_30d",              "Alcohol in past 30 days",               "bin",
  "Health",            "tobacco_any",              "Ever used tobacco",                     "bin",
  "Health",            "any_activity",             "Any physical activity (check coding)",  "bin",
  "Work",              "hours_40plus",             "Works 40+ hours/week",                  "bin",
  "Work",              "hours_50plus",             "Works 50+ hours/week",                  "bin",
  "Work",              "years_10plus",             "More than 10 years in this work",       "bin",
  "Work",              "primary_work_setting",     "Works mostly outdoors",                 "bin",
  "Work",              "heavy_lifting",            "Heavy lifting",                         "bin",
  "Work",              "repetitive_movements",     "Repetitive movements",                  "bin",
  "Work",              "bending_twisting",         "Bending/twisting",                      "bin",
  "Work",              "standing_long_hours",      "Standing for long periods",             "bin",
  "Work",              "heat_conditions",          "Hot conditions",                        "bin",
  "Work",              "cold_wet_conditions",      "Cold/wet conditions",                   "bin",
  "Pesticides",        "pesticide_exposure",       "Pesticides at work",                    "bin",
  "Pesticides",        "pesticides_home",          "Pesticides at home",                    "bin",
  "Pesticides",        "pest_clothes_worry",       "Worried about pesticides on clothes",   "bin",
  "Farmworkers only",  "pesticide_training",       "Received pesticide training*",          "bin",
  "Farmworkers only",  "ppe_always",               "Always uses protective equipment*",     "bin",
  "Farmworkers only",  "pest_worry_any",           "Worried about pesticide health effects*", "bin",
  "Farmworkers only",  "pest_symp_headaches",      "Headache after pesticide exposure*",    "bin",
  "Farmworkers only",  "ag_product_strawberries",  "Works with strawberries*",              "bin",
  "Farmworkers only",  "harvest_or_cut",           "Harvests or cuts*",                     "bin",
  "Farmworkers only",  "time_shower",              "Hours until shower after exposure*",    "cont"
)

# ===================== PART A: BIVARIATE =====================
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
fisher_p <- function(tab) {
  if (min(dim(tab)) < 2) return(NA)
  tryCatch(fisher.test(tab)$p.value,
           error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value)
}

compare <- function(outcome) {
  pmap_dfr(vars, function(group, var, label, type) {
    keep <- !is.na(an[[var]]) & !is.na(an[[outcome]])
    x <- an[[var]][keep]; y <- an[[outcome]][keep]
    n <- as.character(sum(keep))
    if (type == "cont") {
      p  <- suppressWarnings(wilcox.test(x ~ y)$p.value)
      ms <- function(v) sprintf("%.1f (%.1f)", mean(v), sd(v))
      return(tibble(Group = group, var = var, Characteristic = paste0(label, ", mean (SD)"),
                    n = n, No = ms(x[y == 0]), Yes = ms(x[y == 1]), p_num = p))
    }
    p  <- fisher_p(table(x, y))
    cp <- function(k, g) { a <- sum(x == k & y == g); sprintf("%d (%.1f)", a, 100 * a / sum(y == g)) }
    if (type == "bin")
      return(tibble(Group = group, var = var, Characteristic = label, n = n,
                    No = cp(1, 0), Yes = cp(1, 1), p_num = p))
    lev <- levels(an[[var]])
    bind_rows(
      tibble(Group = group, var = var, Characteristic = label, n = n, No = "", Yes = "", p_num = p),
      tibble(Group = group, var = var, Characteristic = paste0("   ", lev), n = "",
             No = map_chr(lev, cp, g = 0), Yes = map_chr(lev, cp, g = 1), p_num = NA)
    )
  })
}

biv <- compare("any_neuro")
nn  <- table(an$any_neuro)
biv_out <- biv %>%
  transmute(Group, Characteristic, n,
            !!paste0("No (n=", nn["0"], ")")  := No,
            !!paste0("Yes (n=", nn["1"], ")") := Yes,
            p = fmt_p(p_num))
print(biv_out, n = Inf, width = Inf)

screened <- biv %>% filter(!is.na(p_num), p_num < 0.20)
cat("\nPASSED SCREENING (p < 0.20):\n")
screened %>% transmute(Characteristic, n, p = fmt_p(p_num)) %>% print()

write_csv(biv_out, "/Users/feliciasong/Desktop/table_any_neuro_bivariate.csv", na = "")

# ===================== PART B: REGRESSION =====================
fit_pr <- function(formula) {
  m  <- glm(formula, family = poisson(link = "log"), data = an)
  se <- sqrt(diag(vcovHC(m, type = "HC0")))
  b  <- coef(m)
  tibble(Variable = names(b), PR = exp(b),
         low = exp(b - 1.96 * se), high = exp(b + 1.96 * se),
         p = 2 * pnorm(-abs(b / se)), n = nobs(m), cases = sum(m$y)) %>%
    filter(Variable != "(Intercept)")
}

# Crude PRs for every variable that passed screening (age as per 10 years)
crude_vars <- screened$var %>% recode(age = "age10") %>% unique()
crude <- map_dfr(crude_vars, ~ fit_pr(reformulate(.x, "any_neuro"))) %>%
  mutate(Model = "Crude", .before = 1)

# Adjusted models: about 10 cases per variable -> up to 4 variables
adj_a <- fit_pr(any_neuro ~ female + age10 + heavy_lifting + indig_lang) %>%
  mutate(Model = "Adjusted A: work", .before = 1)
adj_b <- fit_pr(any_neuro ~ female + age10 + health_cond_diabetes + obese) %>%
  mutate(Model = "Adjusted B: health", .before = 1)

reg_out <- bind_rows(crude, adj_a, adj_b) %>%
  mutate(`PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", PR, low, high),
         p = fmt_p(p)) %>%
  select(Model, Variable, `PR (95% CI)`, p, n, cases)
cat("\nREGRESSION (prevalence ratios):\n")
print(reg_out, n = Inf, width = Inf)

write_csv(reg_out, "/Users/feliciasong/Desktop/table_any_neuro_regression.csv")
