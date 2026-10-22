# ===== Table 1 check: answered (of 136), average, and range for every demographic =====

library(tidyverse)
options(width = 200)

dat <- readRDS("/Users/feliciasong/Desktop/VIIDAI_scored.rds")
N <- nrow(dat)
if (!"bmi" %in% names(dat)) dat$bmi <- dat$weight / (dat$height / 100)^2

# ---- 1. Number variables: answered, mean (SD), median, range ----
num_vars <- c(age          = "Age (years)",
              hh_num       = "Household size",
              children_num = "Number of children (parents only)",
              years_sq     = "Years in San Quintin",
              height       = "Height (cm)",
              weight       = "Weight (kg)",
              bmi          = "BMI (kg/m2)")

num_tab <- imap_dfr(num_vars, function(label, v) {
  x <- as.numeric(dat[[v]])
  tibble(Variable    = label,
         Answered    = sprintf("%d / %d", sum(!is.na(x)), N),
         `Mean (SD)` = sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
         Median      = sprintf("%.1f", median(x, na.rm = TRUE)),
         Range       = sprintf("%.1f to %.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
})

cat("\n===== NUMBER VARIABLES =====\n")
print(as.data.frame(num_tab), right = FALSE, row.names = FALSE)

# ---- 2. Category variables: answered, n (%) per answer, codes seen ----
cat_vars <- list(
  sex             = list("Sex", c("0" = "Female", "1" = "Male")),
  state_origin    = list("State of origin", c("0" = "Baja California", "1" = "Baja California Sur",
                         "2" = "Chiapas", "3" = "Ciudad de Mexico", "4" = "Colima",
                         "5" = "Estado de Mexico", "6" = "Guerrero", "7" = "Michoacan",
                         "8" = "Morelos", "9" = "Oaxaca", "10" = "Sinaloa", "11" = "Sonora",
                         "12" = "Veracruz", "99" = "Another country")),
  occupation      = list("Occupation", c("0" = "Agricultural worker", "1" = "Domestic worker/homemaker",
                         "2" = "Informal commerce", "3" = "Unemployed, looking for work",
                         "4" = "Unemployed, not looking for work", "5" = "Student",
                         "6" = "Construction/Trades", "7" = "Transportation", "8" = "Teacher",
                         "99" = "Other")),
  ethnicity       = list("Ethnicity", c("0" = "Mestizo", "1" = "Indigenous", "2" = "Unknown",
                         "99" = "Other")),
  indig_lang      = list("Speaks an Indigenous language", c("0" = "No", "1" = "Yes")),
  indig_lang_type = list("Which Indigenous language (speakers only)", c("0" = "Mixteco", "1" = "Nahuatl",
                         "2" = "Triqui", "3" = "Zapoteco", "99" = "Other")),
  can_read        = list("Can read", c("0" = "No", "1" = "Yes")),
  can_write       = list("Can write", c("0" = "No", "1" = "Yes")),
  have_children   = list("Has children", c("0" = "No", "1" = "Yes"))
)

cat_tab <- imap_dfr(cat_vars, function(info, v) {
  x <- dat[[v]]
  ans <- sum(!is.na(x))
  seen <- sort(unique(na.omit(x)))
  header <- tibble(Variable = info[[1]],
                   Answered = sprintf("%d / %d", ans, N),
                   `n (%)` = "",
                   `Codes seen` = paste(seen, collapse = ", "))
  answers <- map_dfr(seen, function(k) {
    lab <- info[[2]][as.character(k)]
    k_n <- sum(x == k, na.rm = TRUE)
    tibble(Variable = paste0("   ", ifelse(is.na(lab), paste("UNEXPECTED CODE", k), lab)),
           Answered = "",
           `n (%)` = sprintf("%d (%.1f%%)", k_n, 100 * k_n / ans),
           `Codes seen` = "")
  })
  bind_rows(header, answers)
})

cat("\n===== CATEGORY VARIABLES (% = out of those who answered) =====\n")
print(as.data.frame(cat_tab), right = FALSE, row.names = FALSE)

# ---- 3. Save both for Excel ----
write_csv(bind_rows(num_tab, cat_tab), "/Users/feliciasong/Desktop/table1_check.csv", na = "")
