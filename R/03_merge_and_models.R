# =============================================================================
# 03_merge_and_models.R
# Step 3: Merge CES 2021 survey data with proximity measures
#         and run statistical models
#
# Models:
#   M1 - OLS: thermometer ~ log_dist_nearest + controls
#   M2 - OLS: thermometer ~ log_dist_nearest + cwb_nearest + controls
#   M3 - OLS: thermometer ~ log_dist_nearest * cwb_nearest + controls
#             (key interaction: proximity × CWB)
#   M4 - OLS: thermometer ~ dist_urban + dist_rural + controls
#             (separate urban vs. remote effects)
#   M5 - OLS: resentment_index ~ same as M3 (robustness check)
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

setwd("/home/ral/.openclaw/workspace/urban_proximity")

# =============================================================================
# 1. Load datasets
# =============================================================================
cat("Loading datasets...\n")

# CES 2021 DVs (from Step 1)
ces_dv <- readRDS("data-clean/ces2021_dv.rds")

# Full CES for controls
ces_full <- readRDS("data/ces2021.rds")

# Proximity measures (from Step 2)
prox <- read_csv("data-clean/proximity_fed_enhanced.csv", show_col_types = FALSE)

cat("  CES DV:", nrow(ces_dv), "x", ncol(ces_dv), "\n")
cat("  Proximity:", nrow(prox), "x", ncol(prox), "\n")


# =============================================================================
# 2. Extract and clean control variables from full CES
# =============================================================================
cat("\nPreparing controls...\n")

controls_raw <- ces_full %>%
  select(
    feduid,
    # Demographics
    age          = cps21_age,
    educ         = cps21_education,
    gender       = cps21_genderid,
    born_canada  = cps21_bornin_canada,
    # Ideology (0=left, 10=right)
    ideology_lr  = cps21_lr_scale_bef_1,
    # Income
    income_raw   = cps21_income_number,
    # News consumption
    news_cons    = cps21_news_cons,
    # Vote choice (for robustness)
    vote         = cps21_votechoice
  ) %>%
  mutate(
    # Age: recode -99 / impossible values
    age = ifelse(as.numeric(age) < 18 | as.numeric(age) > 100, NA, as.numeric(age)),

    # Education: 1–11 scale (12 = DK → NA)
    educ_num = case_when(
      as.numeric(educ) %in% 1:11 ~ as.numeric(educ),
      TRUE ~ NA_real_
    ),
    # Binary: university degree or higher
    univ_degree = ifelse(educ_num >= 8, 1L, 0L),

    # Gender: binary male/female + non-binary flag
    male         = ifelse(as.numeric(gender) == 1, 1L, 0L),
    female       = ifelse(as.numeric(gender) == 2, 1L, 0L),
    nonbinary    = ifelse(as.numeric(gender) == 3, 1L, 0L),

    # Born in Canada
    born_canada  = ifelse(as.numeric(born_canada) == 1, 1L, 0L),

    # Ideology: -99 and out-of-range → NA, then recode
    ideology_lr  = case_when(
      as.numeric(ideology_lr) >= 0 & as.numeric(ideology_lr) <= 10 ~
        as.numeric(ideology_lr),
      TRUE ~ NA_real_
    ),

    # Income: trim garbage values (>= 1e+20), keep realistic values
    income_num = case_when(
      as.numeric(income_raw) > 0 & as.numeric(income_raw) < 1e7 ~
        as.numeric(income_raw),
      TRUE ~ NA_real_
    ),
    log_income = log1p(income_num),

    # News consumption: 1–6 ordinal (DK → NA)
    news_cons_num = case_when(
      as.numeric(news_cons) %in% 1:6 ~ as.numeric(news_cons),
      TRUE ~ NA_real_
    )
  ) %>%
  select(feduid, age, educ_num, univ_degree, male, female, nonbinary,
         born_canada, ideology_lr, income_num, log_income, news_cons_num, vote)

cat("  Controls shape:", nrow(controls_raw), "x", ncol(controls_raw), "\n")


# =============================================================================
# 3. Merge everything at the respondent level
# =============================================================================
cat("\nMerging...\n")

# Bind controls by row position — ces_dv and ces_full are the same dataset
# in the same row order, so we column-bind directly (NOT join, which would explode
# because feduid is non-unique: 338 ridings × many respondents each)
stopifnot(nrow(ces_dv) == nrow(controls_raw))
stopifnot(all(ces_dv$feduid == controls_raw$feduid))
ces_combined <- bind_cols(
  ces_dv,
  controls_raw %>% select(-feduid)  # drop duplicate feduid column
)

# Join proximity measures (many respondents per riding)
ces_combined <- ces_combined %>%
  left_join(
    prox %>% select(
      FEDUID,
      # Core proximity measures
      dist_nearest_reserve_km,
      log_dist_nearest_km,
      nearest_reserve_cwb,
      nearest_reserve_urban,
      # CWB-stratified distances
      dist_nearest_cwb_Q1_lowest_km,
      dist_nearest_cwb_Q4_highest_km,
      log_dist_cwb_q1_km,
      log_dist_cwb_q4_km,
      # Urban/rural split
      dist_nearest_urban_cma_km,
      dist_nearest_rural_remote_km,
      log_dist_urban_km,
      log_dist_rural_km,
      # Buffer counts
      reserves_within_50km,
      reserves_within_100km,
      has_reserve_50km,
      log_reserves_50km,
      log_reserves_100km,
      # Province
      PRUID
    ),
    by = c("feduid" = "FEDUID")
  )

cat("  Merged shape:", nrow(ces_combined), "x", ncol(ces_combined), "\n")
cat("  N with proximity data:", sum(!is.na(ces_combined$dist_nearest_reserve_km)), "\n")


# =============================================================================
# 4. Final analytical sample
# =============================================================================
cat("\nBuilding analytical sample...\n")

# Exclude self-identified Indigenous respondents (n=633)
ces_analysis <- ces_combined %>%
  filter(as.numeric(cps21_vismin_4) != 1 | is.na(as.numeric(cps21_vismin_4)))

cat("  N after excluding Indigenous respondents:", nrow(ces_analysis), "\n")

# Recode DV: therm_indigenous already cleaned in Step 1
# Create resentment index (higher = more pro-Indigenous)
ces_analysis <- ces_analysis %>%
  mutate(
    resentment_index = rowMeans(
      cbind(ab_favors_rev, ab_deserve, ab_col),
      na.rm = FALSE  # NA if any item missing — use complete cases only
    )
  )

# Check N for each DV
cat("  N with thermometer DV:", sum(!is.na(ces_analysis$therm_indigenous)), "\n")
cat("  N with resentment index DV:", sum(!is.na(ces_analysis$resentment_index)), "\n")


# =============================================================================
# 5. Province fixed effects preparation
# =============================================================================
# Use province as factor (riding-level clustering)
ces_analysis <- ces_analysis %>%
  mutate(
    province = as.factor(as.numeric(cps21_province)),
    # Standardize continuous IVs for interpretable coefficients
    log_dist_z  = scale(log_dist_nearest_km)[, 1],
    cwb_z       = scale(nearest_reserve_cwb)[, 1],
    age_z       = scale(age)[, 1],
    ideol_z     = scale(ideology_lr)[, 1]
  )

cat("\n  Province distribution:\n")
print(table(ces_analysis$province, useNA = "ifany"))


# =============================================================================
# 6. MODEL 1 — Baseline: Distance → Thermometer (no moderator)
# =============================================================================
cat("\n\n=== MODEL 1: Baseline (thermometer ~ log distance) ===\n")

m1_data <- ces_analysis %>%
  filter(!is.na(therm_indigenous),
         !is.na(log_dist_nearest_km),
         !is.na(ideology_lr),
         !is.na(educ_num),
         !is.na(age),
         !is.na(male))

m1 <- lm(
  therm_indigenous ~
    log_dist_nearest_km +
    ideology_lr +
    educ_num +
    age +
    male +
    born_canada +
    province,
  data = m1_data
)

cat("  N:", nobs(m1), "\n")
print(summary(m1)$coefficients[1:8, ])
cat("  R²:", round(summary(m1)$r.squared, 4),
    "| Adj. R²:", round(summary(m1)$adj.r.squared, 4), "\n")


# =============================================================================
# 7. MODEL 2 — Add CWB score of nearest reserve
# =============================================================================
cat("\n=== MODEL 2: Distance + CWB score of nearest reserve ===\n")

m2_data <- ces_analysis %>%
  filter(!is.na(therm_indigenous),
         !is.na(log_dist_nearest_km),
         !is.na(nearest_reserve_cwb),
         !is.na(ideology_lr),
         !is.na(educ_num),
         !is.na(age),
         !is.na(male))

m2 <- lm(
  therm_indigenous ~
    log_dist_nearest_km +
    nearest_reserve_cwb +
    ideology_lr +
    educ_num +
    age +
    male +
    born_canada +
    province,
  data = m2_data
)

cat("  N:", nobs(m2), "\n")
print(summary(m2)$coefficients[1:9, ])
cat("  R²:", round(summary(m2)$r.squared, 4), "\n")


# =============================================================================
# 8. MODEL 3 — Key interaction: Distance × CWB
# =============================================================================
cat("\n=== MODEL 3: Distance × CWB interaction (MAIN MODEL) ===\n")

m3_data <- m2_data  # Same sample

m3 <- lm(
  therm_indigenous ~
    log_dist_nearest_km * nearest_reserve_cwb +
    ideology_lr +
    educ_num +
    age +
    male +
    born_canada +
    province,
  data = m3_data
)

cat("  N:", nobs(m3), "\n")
coef_m3 <- summary(m3)$coefficients
# Print key terms
key_terms <- c("(Intercept)", "log_dist_nearest_km", "nearest_reserve_cwb",
               "log_dist_nearest_km:nearest_reserve_cwb",
               "ideology_lr", "educ_num", "age", "male", "born_canada")
key_terms_present <- key_terms[key_terms %in% rownames(coef_m3)]
print(coef_m3[key_terms_present, ])
cat("  R²:", round(summary(m3)$r.squared, 4), "\n")

# Interpretation of interaction
b_dist  <- coef(m3)["log_dist_nearest_km"]
b_cwb   <- coef(m3)["nearest_reserve_cwb"]
b_inter <- coef(m3)["log_dist_nearest_km:nearest_reserve_cwb"]
cat("\n  --- INTERACTION INTERPRETATION ---\n")
cat(sprintf("  At low CWB (Q1, ~45): dist effect = %.3f + %.3f * 45 = %.3f\n",
            b_dist, b_inter, b_dist + b_inter * 45))
cat(sprintf("  At high CWB (Q4, ~75): dist effect = %.3f + %.3f * 75 = %.3f\n",
            b_dist, b_inter, b_dist + b_inter * 75))
cat("  (Negative = more distant → lower thermometer; positive = more distant → higher)\n")


# =============================================================================
# 9. MODEL 4 — Urban vs. Remote reserves (separate distance terms)
# =============================================================================
cat("\n=== MODEL 4: Urban vs. Remote reserve distance (separate terms) ===\n")

m4_data <- ces_analysis %>%
  filter(!is.na(therm_indigenous),
         !is.na(log_dist_urban_km),
         !is.na(log_dist_rural_km),
         !is.na(ideology_lr),
         !is.na(educ_num),
         !is.na(age),
         !is.na(male))

m4 <- lm(
  therm_indigenous ~
    log_dist_urban_km +
    log_dist_rural_km +
    ideology_lr +
    educ_num +
    age +
    male +
    born_canada +
    province,
  data = m4_data
)

cat("  N:", nobs(m4), "\n")
key_m4 <- c("(Intercept)", "log_dist_urban_km", "log_dist_rural_km",
            "ideology_lr", "educ_num", "age", "male", "born_canada")
print(summary(m4)$coefficients[key_m4[key_m4 %in% rownames(summary(m4)$coefficients)], ])
cat("  R²:", round(summary(m4)$r.squared, 4), "\n")


# =============================================================================
# 10. MODEL 5 — Resentment Index (robustness check)
# =============================================================================
cat("\n=== MODEL 5: Resentment Index (robustness) ===\n")

m5_data <- ces_analysis %>%
  filter(!is.na(resentment_index),
         !is.na(log_dist_nearest_km),
         !is.na(nearest_reserve_cwb),
         !is.na(ideology_lr),
         !is.na(educ_num),
         !is.na(age),
         !is.na(male))

m5 <- lm(
  resentment_index ~
    log_dist_nearest_km * nearest_reserve_cwb +
    ideology_lr +
    educ_num +
    age +
    male +
    born_canada +
    province,
  data = m5_data
)

cat("  N:", nobs(m5), "\n")
coef_m5 <- summary(m5)$coefficients
key_m5 <- c("(Intercept)", "log_dist_nearest_km", "nearest_reserve_cwb",
             "log_dist_nearest_km:nearest_reserve_cwb",
             "ideology_lr", "educ_num", "age", "male")
key_m5_present <- key_m5[key_m5 %in% rownames(coef_m5)]
print(coef_m5[key_m5_present, ])
cat("  R²:", round(summary(m5)$r.squared, 4), "\n")


# =============================================================================
# 11. Cluster-robust standard errors (cluster by riding)
# =============================================================================
cat("\n=== CLUSTER-ROBUST SEs (cluster by feduid, M3) ===\n")
# Use sandwich package for cluster-robust SEs
if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich", lib = "~/R/library", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("lmtest", quietly = TRUE)) {
  install.packages("lmtest", lib = "~/R/library", repos = "https://cloud.r-project.org")
}
.libPaths(c("~/R/library", .libPaths()))
library(sandwich)
library(lmtest)

m3_vcov_cluster <- vcovCL(m3, cluster = ~ feduid, data = m3_data)
m3_robust <- coeftest(m3, vcov = m3_vcov_cluster)

key_terms_present <- key_terms[key_terms %in% rownames(m3_robust)]
cat("  Key coefficients (cluster-robust SEs):\n")
print(m3_robust[key_terms_present, ])


# =============================================================================
# 12. Save results
# =============================================================================
cat("\nSaving analytical dataset and model objects...\n")

saveRDS(ces_analysis, "data-clean/ces2021_analysis.rds")
saveRDS(list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5),
        "data-clean/models.rds")

# Summary table for output
model_summary <- data.frame(
  Model = c("M1: Baseline", "M2: + CWB", "M3: Distance×CWB", "M4: Urban/Rural split", "M5: Resentment (robust)"),
  DV = c("Thermometer", "Thermometer", "Thermometer", "Thermometer", "Resentment Index"),
  N = c(nobs(m1), nobs(m2), nobs(m3), nobs(m4), nobs(m5)),
  R2 = round(c(summary(m1)$r.squared, summary(m2)$r.squared,
               summary(m3)$r.squared, summary(m4)$r.squared,
               summary(m5)$r.squared), 4)
)
write_csv(model_summary, "output/model_summary.csv")

cat("\n=== STEP 3 COMPLETE ===\n")
print(model_summary)
