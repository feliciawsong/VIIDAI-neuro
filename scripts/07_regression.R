# ===== Step 7: Regression — prevalence ratios (Poisson, robust SE) =====
# One-time install: install.packages("sandwich")

library(tidyverse)
library(sandwich)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

an <- dat %>%
  mutate(
    female          = as.numeric(sex == 0),
    age10           = age / 10,                    # PR per 10 years of age
    obese           = as.numeric(bmi >= 30),
    migraine_yes    = as.numeric(migraine == "Yes"),
    neuropathic_yes = as.numeric(neuropathic == "Yes")
  )

# --- Fit one model, return prevalence ratios ---
fit_pr <- function(formula) {
  m  <- glm(formula, family = poisson(link = "log"), data = an)
  se <- sqrt(diag(vcovHC(m, type = "HC0")))
  b  <- coef(m)
  tibble(Variable = names(b),
         PR   = exp(b),
         low  = exp(b - 1.96 * se),
         high = exp(b + 1.96 * se),
         p    = 2 * pnorm(-abs(b / se)),
         n     = nobs(m),
         cases = sum(m$y)) %>%
    filter(Variable != "(Intercept)")
}

models <- list(
  # Migraine
  "Migraine | crude"                   = migraine_yes ~ female,
  "Migraine | crude "                  = migraine_yes ~ heavy_lifting,
  "Migraine | crude  "                 = migraine_yes ~ alc_ever,
  "Migraine | adjusted"                = migraine_yes ~ female + heavy_lifting,
  "Migraine | alcohol adjusted for sex" = migraine_yes ~ female + alc_ever,
  # Neuropathic pain
  "Neuropathic | crude"                = neuropathic_yes ~ age10,
  "Neuropathic | crude "               = neuropathic_yes ~ health_cond_diabetes,
  "Neuropathic | crude  "              = neuropathic_yes ~ obese,
  "Neuropathic | adjusted (diabetes)"  = neuropathic_yes ~ age10 + health_cond_diabetes,
  "Neuropathic | adjusted (obesity)"   = neuropathic_yes ~ age10 + obese
)

results <- imap_dfr(models, ~ fit_pr(.x) %>% mutate(Model = trimws(.y), .before = 1)) %>%
  mutate(`PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", PR, low, high),
         p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))) %>%
  select(Model, Variable, `PR (95% CI)`, p, n, cases)

print(results, n = Inf, width = Inf)

write_csv(results, "/Users/feliciasong/Desktop/table_regression.csv")
