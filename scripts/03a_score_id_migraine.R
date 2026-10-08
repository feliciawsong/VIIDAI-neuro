# =============================================================================
# Step 3a: Score ID Migraine
# =============================================================================
# Input : your cleaned, recoded spreadsheet (.csv or .xlsx)
# Output: the same data with migraine columns added, saved to data/clean/
#         (Step 3b, the DN4 script, adds its columns to this same file.)
#
# ID Migraine (Lipton et al., 2003): 3 yes/no items about headaches in the
# past 3 months. Yes to >= 2 of 3 = positive migraine screen.
#
# HOW TO USE
#   1. Check the SETTINGS section below (file path, column names).
#   2. In RStudio: Session > Set Working Directory > To Project Directory
#      (the folder that contains "scripts/" and "data/").
#   3. Click "Source". Read the checks printed in the Console.
#   4. Hand-check the 10 printed rows against the paper forms.
#
# Packages (one-time install): install.packages(c("dplyr", "readxl"))
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

# ---- SETTINGS ---------------------------------------------------------------

input_file  <- "data/clean/cleaned_data.xlsx"   # or .csv
output_file <- "data/clean/scored_data.csv"     # a .rds copy is saved too

id_col <- "id"   # participant ID column (only used for the hand-check printout)

# Q4A. In the past 3 months, have you had headaches? (0 = no, 1 = yes)
headache_col <- "headache_3mo"

# Q4B. How frequently have you had headaches in the past 3 months?
#   0 = < once a month, 1 = 1-3 times a month, 2 = once a week,
#   3 = several times a week, 4 = daily
headache_freq_col <- "freq_headache_3mo"

# The 3 ID Migraine items (0 = no, 1 = yes)
idm_cols <- c(
  "headache_activity_limit",  # Q4E. Limited work/study/usual activities
  "headache_nausea",          # Q4C. Nausea / stomach discomfort
  "headache_photophobia"      # Q4D. Light bothered you
)

idm_cutoff <- 2   # published cutoff: >= 2 of 3 = positive

# Stricter screener for a sensitivity analysis. The original ID Migraine
# requires >= 2 headaches in 3 months, but Q4A asks about ANY headache.
# Q4B >= 1 (1-3 times a month) guarantees >= 3 headaches in 3 months.
# "< once a month" can't be told apart from a single headache, so the
# strict version screens it out.
strict_min_freq <- 1

# ---- LOAD DATA --------------------------------------------------------------

if (grepl("\\.xlsx?$", input_file, ignore.case = TRUE)) {
  dat <- readxl::read_excel(input_file)
} else {
  dat <- read.csv(input_file, stringsAsFactors = FALSE)
}
dat <- as.data.frame(dat)
cat("Loaded", nrow(dat), "rows from", input_file, "\n\n")

# ---- CHECKS -----------------------------------------------------------------

missing_cols <- setdiff(c(id_col, headache_col, headache_freq_col, idm_cols), names(dat))
if (length(missing_cols) > 0) {
  stop("These columns are not in your spreadsheet (check spelling in SETTINGS):\n  ",
       paste(missing_cols, collapse = ", "))
}

check_coding <- function(cols, allowed) {
  bad <- cols[sapply(cols, function(col) any(!is.na(dat[[col]]) & !(dat[[col]] %in% allowed)))]
  for (col in bad) {
    cat("Column", col, "has values outside", paste(allowed, collapse = "/"), "or blank:\n")
    print(table(dat[[col]], useNA = "ifany"))
  }
  bad
}
bad <- c(check_coding(c(headache_col, idm_cols), c(0, 1)),
         check_coding(headache_freq_col, 0:4))
if (length(bad) > 0) stop("Fix the coding in the columns above, then re-run.")
cat("Check passed: columns found and coded correctly.\n")

# Skip-logic check: people who said NO headaches shouldn't have "yes" answers
# on the follow-up questions. These rows are flagged, not changed.
no_headache <- dat[[headache_col]] %in% 0
followup_yes <- rowSums(dat[, idm_cols] == 1, na.rm = TRUE) > 0
inconsistent <- no_headache & followup_yes
if (any(inconsistent)) {
  cat("\nWARNING:", sum(inconsistent),
      "people said NO headaches (Q4A = 0) but YES to a follow-up item.",
      "They are scored as migraine 'No'. Check these IDs against the forms:\n")
  print(dat[inconsistent, id_col])
}

# ---- SCORE ------------------------------------------------------------------
# Only people with headaches (Q4A = 1) are scored. People without headaches
# get NA on idm_score ("not applicable") but count as migraine "No" for the
# overall prevalence. A score is only computed when all 3 items were answered.

dat <- dat %>%
  mutate(
    idm_n_answered = rowSums(!is.na(across(all_of(idm_cols)))),
    idm_score = if_else(.data[[headache_col]] %in% 1 & idm_n_answered == length(idm_cols),
                        rowSums(across(all_of(idm_cols))),
                        NA_real_),

    # Main definition: Q4A = yes and >= 2 of 3 items
    migraine = case_when(
      .data[[headache_col]] == 0 ~ "No",
      idm_score >= idm_cutoff    ~ "Yes",
      idm_score <  idm_cutoff    ~ "No",
      TRUE                       ~ NA_character_   # Q4A missing, or items incomplete
    ),
    migraine = factor(migraine, levels = c("No", "Yes")),

    # Sensitivity definition: also requires headaches >= 1-3 times a month
    migraine_strict = case_when(
      migraine == "No"                                   ~ "No",
      migraine == "Yes" & .data[[headache_freq_col]] >= strict_min_freq ~ "Yes",
      migraine == "Yes" & .data[[headache_freq_col]] <  strict_min_freq ~ "No",
      TRUE                                               ~ NA_character_   # freq missing
    ),
    migraine_strict = factor(migraine_strict, levels = c("No", "Yes"))
  )

# ---- SUMMARY ----------------------------------------------------------------

cat("\n================ ID MIGRAINE SUMMARY ================\n")

cat("\nQ4A. Headaches in past 3 months:\n")
print(table(headache_3mo = dat[[headache_col]], useNA = "ifany"))

cat("\nQ4B. Frequency (among people with headaches):\n")
print(table(freq = dat[[headache_freq_col]][dat[[headache_col]] %in% 1], useNA = "ifany"))

cat("\nEach ID Migraine item (among people with headaches):\n")
for (col in idm_cols) {
  cat(" ", col, ": ")
  print(table(dat[[col]][dat[[headache_col]] %in% 1], useNA = "ifany"))
}

n_incomplete <- sum(dat[[headache_col]] %in% 1 & dat$idm_n_answered < length(idm_cols))
cat("\nPeople with headaches but incomplete items (not scored):", n_incomplete, "\n")

cat("\nID Migraine score, 0-3 (people with headaches):\n")
print(table(idm_score = dat$idm_score, useNA = "ifany"))

cat("\nMIGRAINE (main definition):\n")
print(table(dat$migraine, useNA = "ifany"))

cat("\nMIGRAINE (strict definition, sensitivity analysis):\n")
print(table(dat$migraine_strict, useNA = "ifany"))

cat("\nMigraine by headache frequency (people with headaches):\n")
with_headache <- dat[[headache_col]] %in% 1
print(table(freq = dat[[headache_freq_col]][with_headache],
            migraine = dat$migraine[with_headache], useNA = "ifany"))

# ---- HAND-CHECK: compare 10 random rows to the paper forms -------------------

set.seed(2026)
check_rows <- dat[sample(nrow(dat), min(10, nrow(dat))), ]
cat("\n================ HAND-CHECK THESE 10 ROWS ================\n")
print(check_rows[, c(id_col, headache_col, headache_freq_col, idm_cols,
                     "idm_score", "migraine", "migraine_strict")],
      row.names = FALSE)

# ---- SAVE -------------------------------------------------------------------

dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write.csv(dat, output_file, row.names = FALSE)
saveRDS(dat, sub("\\.csv$", ".rds", output_file))  # .rds keeps factor levels
cat("\nSaved scored data to", output_file, "(and .rds)\n")
