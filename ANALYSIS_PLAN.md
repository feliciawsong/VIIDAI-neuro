# Analysis Plan: Migraine & Neuropathic Pain Prevalence in a Farm-Laborer Community (Baja California)

**Research question:** Who is more likely to have migraine and neuropathic pain, and what factors are associated with it, so we can design targeted community interventions?

**Study design:** Analytical cross-sectional study.

---

## Key decisions

### 1. R or Python? → **R recommended**
| | R | Python |
|---|---|---|
| "Table 1" (n/%, mean/SD by group, with p-values) | `gtsummary` makes publication-ready tables in a few lines | Possible (`tableone`), but more manual |
| Prevalence + 95% CI, prevalence ratios, regression | Standard in epi; `epitools`, `sandwich`, `glm` | `statsmodels` works fine |
| Spanish/indigenous-language reports, figures | `ggplot2`, Quarto | `matplotlib`/`seaborn`, Jupyter |
| What epi reviewers expect | Very common | Less common in epi |

Pick Python only if you already know it well and R would slow you down. Either way: **RStudio + a Quarto (.qmd) file** or **Jupyter notebook**, so code, output, and notes live together.

### 2. What "analytical cross-sectional" means
- **Cross-sectional:** everyone is measured once, at a single point in time (a "snapshot").
- **Descriptive** cross-sectional = *how common is it?* (prevalence only).
- **Analytical** cross-sectional = prevalence **plus** *comparing groups* to find factors **associated** with the outcome (e.g., are women / older workers / pesticide-exposed workers more likely to have neuropathic pain?).
- **Main limitation:** exposure and outcome are measured at the same time, so we can't tell which came first. Say "associated with," not "causes" or "risk of developing."
- **Measure of association:** prevalence ratio (PR) or prevalence odds ratio (POR). If an outcome is common (>10%), the odds ratio overstates the effect, so prefer PR (Poisson regression with robust SEs, or log-binomial).

### 3. "Multivariate" vs. "multivariable"
One outcome with many predictors is **multivariable** regression. (*Multivariate* means multiple outcomes at once.) Reviewers notice this.

---

## Instrument scoring

### DN4 (Douleur Neuropathique 4), Spanish version (Pérez et al., 2007)
- 10 yes/no items (yes = 1, no = 0):
  - **Interview (7):** burning, painful cold, electric shocks, tingling, pins and needles, numbness, itching
  - **Exam (3):** hypoesthesia to touch, hypoesthesia to pinprick, pain caused by brushing
- **Total 0–10; ≥4 = likely neuropathic pain.**
- If the exam wasn't done, use **DN4-interview (7 items), cutoff ≥3**. → *Need to confirm which one we have.*
- DN4 only applies to people **with pain**. People without pain should be "not applicable," not "0" and not "missing."

### ID Migraine (Lipton et al., 2003)
- Screener: ≥2 headaches in the past 3 months (check whether this was asked)
- 3 yes/no items about headaches in the past 3 months:
  1. Activity limitation (disability) for ≥1 day
  2. Nausea
  3. Light bothering you (photophobia)
- **≥2 of 3 = positive migraine screen.**
- Same rule: no headaches → not applicable.

---

## Outcome groupings
- **Pain:** yes / no
- **Neuropathic pain (DN4+):** among those with pain → three-level variable: *No pain / Non-neuropathic pain / Neuropathic pain*
- **Migraine (ID Migraine+):** yes / no
- Optional: overlap (migraine + neuropathic pain)

---

## Candidate risk factors (keep only what we actually collected)
**Sociodemographic:** age, sex, indigenous identity / language (e.g., Mixteco, Triqui, Zapoteco), migration status / state of origin, education, literacy, marital status, income, household size, food insecurity, health insurance (IMSS/IMSS-Bienestar), healthcare access

**Occupational:** years in agriculture, crop type, hours/day, days/week, piece-rate vs. hourly pay, stooping/kneeling, heavy lifting, repetitive hand work, **pesticide exposure / PPE use**, heat exposure, water/shade/break access, prior work injury

**Clinical / behavioral:** **diabetes / high blood sugar** (a major cause of neuropathy), BMI, hypertension, alcohol, tobacco, sleep hours/quality, depression/anxiety/stress, skipped meals, dehydration, caffeine, menstrual-related headache (migraine), pain duration/location/intensity

---

## Step-by-step game plan with time estimates
*Estimates assume part-time work (~2–3 hrs/day) and a few hundred participants.*

| # | Step | What it includes | Est. time |
|---|------|------------------|-----------|
| 0 | **Setup** | Install R + RStudio (or Python); set up folder structure (`data/raw`, `data/clean`, `scripts`, `output`); **keep identifiable data off GitHub** (`.gitignore` the data folder) | 0.5–1 day |
| 1 | **Data audit / codebook** | List every column, what it means, its coding (1/0, Sí/No, text), and allowed values; flag messy entries; confirm DN4 version and ID Migraine screener | 1–2 days |
| 2 | **Cleaning** | Standardize yes/no coding, fix typos, recode categories, build derived variables (age groups, BMI, years worked), handle missing data (report it, don't silently drop) | 2–4 days |
| 3 | **Score DN4 & ID Migraine** | Compute totals and positive/negative flags; hand-check 5–10 rows against the paper forms | 0.5–1 day |
| 4 | **Prevalence** | Prevalence of pain, neuropathic pain, migraine **with 95% CIs**, overall and by sex | 1 day |
| 5 | **Descriptive profile (Table 1)** | Categorical → n (%); continuous → mean (SD) if roughly normal, median (IQR) if skewed. Columns: overall / pain vs. no pain / neuropathic vs. not / migraine vs. not | 1–2 days |
| 6 | **Bivariate analysis** | Chi-square (Fisher if small cells) for categorical; t-test / Mann-Whitney (2 groups) or ANOVA / Kruskal-Wallis (3 groups) for continuous; crude PRs with 95% CI | 2–3 days |
| 7 | **Check the numbers, then pick a model** | Count outcome cases → ~**10 cases per predictor** sets the max number of variables (e.g., 40 neuropathic cases ≈ 4 predictors). Choose variables using theory (draw a simple DAG) + bivariate p < 0.20 | 1 day |
| 8 | **Multivariable regression** | Poisson with robust SEs (adjusted PRs) or logistic (adjusted ORs); check collinearity; sensitivity analyses | 3–5 days |
| 9 | **Figures & interpretation** | Forest plot of adjusted PRs, prevalence bar charts; answer "who and why"; list limitations | 2–3 days |
| 10 | **Community deliverables brainstorm** | Tie each key finding to an action (see below) | 1 session, revisit after step 9 |
| 11 | **Write-up** | Methods and results drafts; Spanish-language community summary | 1–2 weeks |

**Total: about 4–6 weeks part-time** (steps 1–3 are where projects usually get stuck, so budget generously).

---

## Community deliverables: brainstorm starters (refine after results)
- **Return results** in Spanish (and indigenous languages if possible): community meeting, one-page infographic
- **Health talks (pláticas)** with promotoras de salud: what migraine and neuropathic pain are, warning signs, when to seek care
- **Diabetes / glucose screening** and referral if neuropathy is linked to metabolic risk
- **Pesticide safety & PPE** education if exposure is associated
- **Ergonomics / stretching** and heat-stress / hydration education
- **Headache diaries** and trigger identification (sleep, meals, hydration, heat)
- **Referral map** of local clinics (IMSS-Bienestar, community clinics) and low-cost treatment options
- **Policy brief** for employers / local health authorities (shade, water, breaks)

---

## Info needed before step 1
- [ ] Sample size (total n, and roughly how many have pain / headaches)
- [ ] The spreadsheet's column list (de-identified)
- [ ] Was the DN4 physical-exam portion done?
- [ ] Was the ID Migraine "≥2 headaches in 3 months" screener asked?
- [ ] Sampling method (convenience vs. random / census)
- [ ] Your comfort level with R vs. Python
