# ===== Step 12: Women and intersections (sex x work x Indigenous language) =====
# 1. Who reports heavy lifting and heat, by sex and job (work-question sample)
# 2. Migraine by exertion/heat score among WOMEN ONLY, with PR adjusted for Indigenous language
# 3-4. Migraine and neuropathic pain counts by sex x job and sex x Indigenous language

library(tidyverse)
library(sandwich)
dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")

an <- dat %>% mutate(
  migraine_yes    = as.numeric(migraine == "Yes"),
  neuropathic_yes = as.numeric(neuropathic == "Yes"),
  sex_lab         = ifelse(sex == 0, "Women", "Men"),
  job             = case_when(occupation == 0 ~ "Agricultural",
                              occupation == 1 ~ "Domestic/homemaker",
                              !is.na(occupation) ~ "Other/unemployed"),
  exertion_heat   = heavy_lifting + heat_conditions)

cat("\n== 1. Who reports heavy lifting and heat, by sex and job ==\n")
an %>% filter(!is.na(exertion_heat)) %>% group_by(sex_lab, job) %>%
  summarise(people = n(), heavy_lifting = sum(heavy_lifting), heat = sum(heat_conditions), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n== 2. Migraine by exertion/heat score, WOMEN ONLY ==\n")
w <- an %>% filter(sex_lab == "Women", !is.na(exertion_heat), !is.na(indig_lang))
w %>% group_by(exertion_heat) %>%
  summarise(people = n(), migraine = sum(migraine_yes), pct = round(100 * mean(migraine_yes), 1)) %>%
  as.data.frame() %>% print(row.names = FALSE)
m  <- glm(migraine_yes ~ exertion_heat + indig_lang, family = poisson(link = "log"), data = w)
b  <- coef(m)["exertion_heat"]; se <- sqrt(vcovHC(m, type = "HC0")["exertion_heat", "exertion_heat"])
cat(sprintf("Women only, per exposure, adjusted for Indigenous language: PR %.2f (%.2f-%.2f), p = %.3f\n",
            exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se), 2 * pnorm(-abs(b / se))))

cat("\n== 3. Migraine and neuropathic pain by sex and job ==\n")
an %>% group_by(sex_lab, job) %>%
  summarise(people = n(), migraine = sum(migraine_yes), neuropathic = sum(neuropathic_yes), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n== 4. Migraine and neuropathic pain by sex and Indigenous language ==\n")
an %>% group_by(sex_lab, indig_lang) %>%
  summarise(people = n(), migraine = sum(migraine_yes), neuropathic = sum(neuropathic_yes), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)
