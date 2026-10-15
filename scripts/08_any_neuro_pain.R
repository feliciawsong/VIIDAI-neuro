# ===== Step 8: Combined outcome — any neurological pain (migraine OR neuropathic) =====

library(tidyverse)
library(sandwich)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

an <- dat %>%
  mutate(
    any_neuro    = as.numeric(migraine == "Yes" | neuropathic == "Yes"),
    female       = as.numeric(sex == 0),
    age10        = age / 10,
    obese        = as.numeric(bmi >= 30),
    hours_50plus = as.numeric(occupation_hours == 4),
    years_10plus = as.numeric(occupation_years == 4)
  )

# --- 1. Prevalence ---
cat("\nANY NEUROLOGICAL PAIN (migraine or neuropathic):\n")
k <- sum(an$any_neuro); n <- nrow(an); ci <- binom.test(k, n)$conf.int
cat(sprintf("%d / %d = %.1f%% (95%% CI %.1f-%.1f)\n", k, n, 100 * k / n, 100 * ci[1], 100 * ci[2]))

# --- 2. Prevalence ratio helper ---
fit_pr <- function(formula) {
  m  <- glm(formula, family = poisson(link = "log"), data = an)
  se <- sqrt(diag(vcovHC(m, type = "HC0")))
  b  <- coef(m)
  tibble(Variable = names(b), PR = exp(b),
         low = exp(b - 1.96 * se), high = exp(b + 1.96 * se),
         p = 2 * pnorm(-abs(b / se)), n = nobs(m), cases = sum(m$y)) %>%
    filter(Variable != "(Intercept)")
}
fmt <- function(df) df %>%
  mutate(`PR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", PR, low, high),
         p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))) %>%
  select(Model, Variable, `PR (95% CI)`, p, n, cases)

# --- 3. Crude prevalence ratios, one variable at a time ---
crude_vars <- c("female", "age10", "indig_lang", "health_cond_diabetes",
                "health_cond_hypertension", "obese", "alc_ever",
                "heavy_lifting", "repetitive_movements", "heat_conditions",
                "hours_50plus", "years_10plus", "pesticide_exposure", "pesticides_home")

crude <- map_dfr(crude_vars, ~ fit_pr(reformulate(.x, "any_neuro"))) %>%
  mutate(Model = "Crude", .before = 1)

# --- 4. Adjusted models (about 10 cases per variable -> 4 variables) ---
adj_a <- fit_pr(any_neuro ~ female + age10 + heavy_lifting + indig_lang) %>%
  mutate(Model = "Adjusted A: work", .before = 1)
adj_b <- fit_pr(any_neuro ~ female + age10 + health_cond_diabetes + obese) %>%
  mutate(Model = "Adjusted B: health", .before = 1)

results <- fmt(bind_rows(crude, adj_a, adj_b))
print(results, n = Inf, width = Inf)

write_csv(results, "/Users/feliciasong/Desktop/table_any_neuro.csv")
