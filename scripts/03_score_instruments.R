# =============================================================================
# Step 3: Score DN4 (neuropathic pain) and ID Migraine
# =============================================================================
# Input : your cleaned, recoded spreadsheet (.csv or .xlsx)
# Output: the same data with score columns added, saved to data/clean/
#
# Assumes the instrument items are coded 1 = yes (sí), 0 = no, blank/NA = missing.
#
# HOW TO USE
#   1. Edit the SETTINGS section below so the column names match your sheet.
#   2. In RStudio: Session > Set Working Directory > To Project/Source File
#      location (the folder that contains "scripts/" and "data/").
#   3. Click "Source". Read the checks printed in the Console.
#   4. Hand-check the 10 printed rows against the paper forms.
#
# Packages (one-time install): install.packages(c("dplyr", "readxl"))
# =============================================================================

library(dplyr)

# ---- SETTINGS: edit these to match your spreadsheet -------------------------

input_file  <- "data/clean/cleaned_data.xlsx"   # or .csv
output_file <- "data/clean/scored_data.csv"

# Participant ID column (used for the hand-check printout)
id_col <- "id"

# Pain screener: does the participant currently have pain? (1 = yes, 0 = no)
pain_col <- "pain"

# DN4 interview items (7). Order doesn't matter, only the names.
dn4_interview_cols <- c(
  "dn4_burning",          # 1. Quemazón / ardor
  "dn4_painful_cold",     # 2. Sensación de frío doloroso
  "dn4_electric_shocks",  # 3. Descargas eléctricas
  "dn4_tingling",         # 4. Hormigueo
  "dn4_pins_needles",     # 5. Pinchazos
  "dn4_numbness",         # 6. Entumecimiento
  "dn4_itching"           # 7. Escozor / picazón
)

# DN4 physical-exam items (3). Set to NULL if the exam was NOT done:
#   dn4_exam_cols <- NULL
dn4_exam_cols <- c(
  "dn4_hypo_touch",       # 8. Hipoestesia al tacto
  "dn4_hypo_pinprick",    # 9. Hipoestesia al pinchazo
  "dn4_brushing"          # 10. Dolor provocado por el roce
)

# ID Migraine headache screener (>= 2 headaches in past 3 months, 1 = yes).
# Set to NULL if it was not asked. Without a screener, everyone is scored, so
# anyone who left the 3 items blank ends up "missing" rather than "No". If
# people without headaches skipped those items, point this at whatever
# column says they have headaches instead.
headache_col <- "headache_screen"

# ID Migraine items (3), all about headaches in the past 3 months
idm_cols <- c(
  "idm_disability",       # Limited activities for >= 1 day
  "idm_nausea",           # Nausea / upset stomach
  "idm_photophobia"       # Light bothered you
)

# Published cutoffs (don't change unless you have a reason)
dn4_full_cutoff      <- 4   # DN4 10-item total >= 4 -> neuropathic
dn4_interview_cutoff <- 3   # DN4 interview-only (7 items) >= 3 -> neuropathic
idm_cutoff           <- 2   # ID Migraine >= 2 of 3 -> positive

# ---- LOAD DATA --------------------------------------------------------------

if (grepl("\\.xlsx?$", input_file, ignore.case = TRUE)) {
  dat <- readxl::read_excel(input_file)
} else {
  dat <- read.csv(input_file, stringsAsFactors = FALSE)
}
dat <- as.data.frame(dat)
cat("Loaded", nrow(dat), "rows and", ncol(dat), "columns from", input_file, "\n\n")

# ---- CHECK: columns exist and are coded 0/1 ---------------------------------

dn4_cols <- c(dn4_interview_cols, dn4_exam_cols)
all_item_cols <- c(id_col, pain_col, dn4_cols, headache_col, idm_cols)

missing_cols <- setdiff(all_item_cols, names(dat))
if (length(missing_cols) > 0) {
  stop("These columns are not in your spreadsheet (check spelling in SETTINGS):\n  ",
       paste(missing_cols, collapse = ", "))
}

binary_cols <- setdiff(all_item_cols, id_col)
bad_coding <- binary_cols[sapply(binary_cols, function(col) {
  any(!is.na(dat[[col]]) & !(dat[[col]] %in% c(0, 1)))
})]
if (length(bad_coding) > 0) {
  for (col in bad_coding) {
    cat("Column", col, "has values other than 0/1/blank:\n")
    print(table(dat[[col]], useNA = "ifany"))
  }
  stop("Recode the columns above to 1 = yes, 0 = no before scoring.")
}
cat("Check passed: all instrument columns found and coded 0/1/blank.\n\n")

# ---- SCORE DN4 --------------------------------------------------------------
# DN4 is only asked of people WITH pain. People without pain get NA on the
# score ("not applicable"), but count as "not neuropathic" in the overall
# prevalence. A score is only computed when every DN4 item was answered.

has_pain <- dat[[pain_col]] == 1

dat <- dat %>%
  mutate(
    dn4_n_answered = rowSums(!is.na(across(all_of(dn4_cols)))),
    dn4_complete   = has_pain & dn4_n_answered == length(dn4_cols),

    dn4_interview_score = if_else(dn4_complete,
                                  rowSums(across(all_of(dn4_interview_cols))),
                                  NA_real_)
  )

if (!is.null(dn4_exam_cols)) {
  dat <- dat %>%
    mutate(
      dn4_total_score = if_else(dn4_complete,
                                rowSums(across(all_of(dn4_cols))),
                                NA_real_),
      dn4_positive = dn4_total_score >= dn4_full_cutoff
    )
  cat("DN4 scored with all 10 items (cutoff >= ", dn4_full_cutoff, ").\n", sep = "")
} else {
  dat <- dat %>%
    mutate(dn4_positive = dn4_interview_score >= dn4_interview_cutoff)
  cat("DN4 scored with interview items only (cutoff >= ",
      dn4_interview_cutoff, ").\n", sep = "")
}

# Three-group outcome for Table 1: No pain / Non-neuropathic / Neuropathic
dat <- dat %>%
  mutate(
    pain_group = case_when(
      .data[[pain_col]] == 0 ~ "No pain",
      dn4_positive == FALSE  ~ "Non-neuropathic pain",
      dn4_positive == TRUE   ~ "Neuropathic pain",
      TRUE                   ~ NA_character_   # pain missing, or DN4 incomplete
    ),
    pain_group = factor(pain_group,
                        levels = c("No pain", "Non-neuropathic pain", "Neuropathic pain")),

    # Yes/No neuropathic pain for the whole sample (no pain counts as "No")
    neuropathic_pain = case_when(
      pain_group == "Neuropathic pain" ~ "Yes",
      !is.na(pain_group)               ~ "No"
    ),
    neuropathic_pain = factor(neuropathic_pain, levels = c("No", "Yes"))
  )

# ---- SCORE ID MIGRAINE ------------------------------------------------------
# Only people who screen in for headaches are scored. People without
# headaches get NA on the score, but count as migraine "No".

if (!is.null(headache_col)) {
  screened_in  <- dat[[headache_col]] == 1
  screened_out <- dat[[headache_col]] == 0
} else {
  screened_in  <- rep(TRUE, nrow(dat))
  screened_out <- rep(FALSE, nrow(dat))
}

dat <- dat %>%
  mutate(
    idm_n_answered = rowSums(!is.na(across(all_of(idm_cols)))),
    idm_score = if_else(screened_in & idm_n_answered == length(idm_cols),
                        rowSums(across(all_of(idm_cols))),
                        NA_real_),
    migraine = case_when(
      idm_score >= idm_cutoff ~ "Yes",
      idm_score <  idm_cutoff ~ "No",
      screened_out            ~ "No",
      TRUE                    ~ NA_character_   # screener missing or items incomplete
    ),
    migraine = factor(migraine, levels = c("No", "Yes"))
  )

# ---- SUMMARY ----------------------------------------------------------------

cat("\n================ SCORING SUMMARY ================\n")

cat("\nPain screener:\n")
print(table(Pain = dat[[pain_col]], useNA = "ifany"))

n_pain_incomplete <- sum(has_pain & !dat$dn4_complete, na.rm = TRUE)
cat("\nPeople with pain but an incomplete DN4 (not scored):", n_pain_incomplete, "\n")

cat("\nDN4 score distribution (people with pain):\n")
if (!is.null(dn4_exam_cols)) {
  print(table(DN4_total = dat$dn4_total_score, useNA = "ifany"))
} else {
  print(table(DN4_interview = dat$dn4_interview_score, useNA = "ifany"))
}

cat("\nPain group:\n")
print(table(dat$pain_group, useNA = "ifany"))

cat("\nID Migraine score distribution (people who screened in):\n")
print(table(IDM_score = dat$idm_score, useNA = "ifany"))
n_idm_incomplete <- sum(screened_in & dat$idm_n_answered < length(idm_cols), na.rm = TRUE)
cat("People who screened in but have incomplete ID Migraine (not scored):",
    n_idm_incomplete, "\n")

cat("\nMigraine (ID Migraine positive):\n")
print(table(dat$migraine, useNA = "ifany"))

cat("\nOverlap: neuropathic pain x migraine\n")
print(table(Neuropathic = dat$neuropathic_pain, Migraine = dat$migraine, useNA = "ifany"))

# ---- HAND-CHECK: compare 10 random rows to the paper forms -------------------

set.seed(2026)
check_rows <- dat[sample(nrow(dat), min(10, nrow(dat))), ]
score_cols <- intersect(
  c(id_col, pain_col, "dn4_interview_score", "dn4_total_score", "pain_group",
    headache_col, "idm_score", "migraine"),
  names(dat)
)
cat("\n================ HAND-CHECK THESE 10 ROWS ================\n")
print(check_rows[, score_cols], row.names = FALSE)

# ---- SAVE -------------------------------------------------------------------

dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write.csv(dat, output_file, row.names = FALSE)
saveRDS(dat, sub("\\.csv$", ".rds", output_file))  # .rds keeps factor levels
cat("\nSaved scored data to", output_file, "(and .rds)\n")
