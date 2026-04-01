# =============================================================================
# 03b_models_revised.R
# Step 3 (revised): Models with corrected centroids + area as density proxy
#   - Uses proximity_fed_corrected.csv (reserve-subtracted centroids)
#   - Excludes Indigenous respondents (cps21_vismin_4 == 1)
#   - Adds log(riding non-reserve area) as population density proxy
#   - Cluster-robust SEs by federal riding throughout
# =============================================================================

library(haven)
library(dplyr)
library(readr)
library(sandwich)
library(lmtest)

setwd("/home/ral/.openclaw/workspace/urban_proximity")

# =============================================================================
# 1. Load data
# =============================================================================
cat("Loading data...\n")
ces_full <- readRDS("data/ces2021.rds")
ces_dv   <- readRDS("data-clean/ces2021_dv.rds")
prox     <- read_csv("data-clean/proximity_fed_corrected.csv", show_col_types=FALSE)

cat("  CES:", nrow(ces_dv), "| Proximity:", nrow(prox), "ridings\n")

# =============================================================================
# 2. Build analytical dataset
# =============================================================================
cat("Building analytical dataset...\n")

# Extract controls from full CES (same row order as ces_dv)
controls <- ces_full |>
  transmute(
    feduid,
    age         = ifelse(as.numeric(cps21_age) >= 18 & as.numeric(cps21_age) <= 100,
                         as.numeric(cps21_age), NA),
    educ_num    = ifelse(as.numeric(cps21_education) %in% 1:11,
                         as.numeric(cps21_education), NA),
    univ_degree = ifelse(educ_num >= 8, 1L, 0L),
    male        = ifelse(as.numeric(cps21_genderid) == 1, 1L, 0L),
    ideology_lr = ifelse(as.numeric(cps21_lr_scale_bef_1) >= 0 &
                           as.numeric(cps21_lr_scale_bef_1) <= 10,
                         as.numeric(cps21_lr_scale_bef_1), NA),
    born_canada = ifelse(as.numeric(cps21_bornin_canada) == 1, 1L, 0L),
    news_cons   = ifelse(as.numeric(cps21_news_cons) %in% 1:6,
                         as.numeric(cps21_news_cons), NA),
    indigenous  = ifelse(as.numeric(cps21_vismin_4) == 1, 1L, 0L)
  )

stopifnot(nrow(ces_dv) == nrow(controls))
stopifnot(all(ces_dv$feduid == controls$feduid))

ces_merged <- bind_cols(
  ces_dv,
  controls |> select(-feduid)
) |>
  # Join proximity (many respondents per riding — correct many:1)
  left_join(
    prox |>
      mutate(FEDUID = as.numeric(FEDUID)) |>
      select(FEDUID, dist_nearest_reserve_km, log_dist_nearest_km,
             nearest_reserve_cwb, nearest_reserve_urban, nearest_reserve_pop,
             dist_q_Q1_lowest_km, dist_q_Q4_highest_km,
             area_nonres_km2, log_area_nonres_km2, centroid_shift_km),
    by = c("feduid" = "FEDUID")
  )

# =============================================================================
# 3. Analytical sample: EXCLUDE Indigenous respondents
# =============================================================================
cat("Excluding Indigenous respondents...\n")
n_before <- nrow(ces_merged)
ces_analysis <- ces_merged |>
  filter(indigenous != 1 | is.na(indigenous)) |>
  mutate(
    province = as.factor(as.numeric(cps21_province)),
    # Resentment index
    resentment_index = rowMeans(cbind(ab_favors_rev, ab_deserve, ab_col), na.rm=FALSE),
    # Tertiles of riding area (urbanicity proxy)
    area_tertile = cut(log_area_nonres_km2,
                       breaks = quantile(log_area_nonres_km2, probs=c(0,.33,.67,1), na.rm=TRUE),
                       labels = c("small_urban","medium","large_remote"),
                       include.lowest = TRUE)
  )

cat(sprintf("  Before: %d | After: %d | Excluded: %d Indigenous respondents\n",
            n_before, nrow(ces_analysis), n_before - nrow(ces_analysis)))
cat("  N with thermometer:", sum(!is.na(ces_analysis$therm_indigenous)), "\n")
cat("  N with resentment index:", sum(!is.na(ces_analysis$resentment_index)), "\n")

# =============================================================================
# 4. MODEL A — Baseline with area control
# =============================================================================
cat("\n=== MODEL A: Baseline + area proxy ===\n")
mA_data <- ces_analysis |>
  filter(!is.na(therm_indigenous), !is.na(log_dist_nearest_km),
         !is.na(log_area_nonres_km2), !is.na(ideology_lr),
         !is.na(educ_num), !is.na(age), !is.na(male))

mA <- lm(therm_indigenous ~
           log_dist_nearest_km +
           log_area_nonres_km2 +
           ideology_lr + educ_num + age + male + born_canada + province,
         data = mA_data)

mA_robust <- coeftest(mA, vcovCL(mA, cluster = ~feduid, data = mA_data))
cat("  N:", nobs(mA), "\n")
key <- c("log_dist_nearest_km","log_area_nonres_km2","ideology_lr","educ_num","age","male","born_canada")
print(round(mA_robust[key[key %in% rownames(mA_robust)],], 4))
cat("  R²:", round(summary(mA)$r.squared, 4), "\n")

# =============================================================================
# 5. MODEL B — CWB interaction + area control (MAIN MODEL)
# =============================================================================
cat("\n=== MODEL B: Distance × CWB + area proxy (MAIN MODEL) ===\n")
mB_data <- ces_analysis |>
  filter(!is.na(therm_indigenous), !is.na(log_dist_nearest_km),
         !is.na(nearest_reserve_cwb), !is.na(log_area_nonres_km2),
         !is.na(ideology_lr), !is.na(educ_num), !is.na(age), !is.na(male))

mB <- lm(therm_indigenous ~
           log_dist_nearest_km * nearest_reserve_cwb +
           log_area_nonres_km2 +
           ideology_lr + educ_num + age + male + born_canada + province,
         data = mB_data)

mB_robust <- coeftest(mB, vcovCL(mB, cluster = ~feduid, data = mB_data))
cat("  N:", nobs(mB), "\n")
key_B <- c("log_dist_nearest_km","nearest_reserve_cwb",
           "log_dist_nearest_km:nearest_reserve_cwb","log_area_nonres_km2",
           "ideology_lr","educ_num","age","male")
print(round(mB_robust[key_B[key_B %in% rownames(mB_robust)],], 4))
cat("  R²:", round(summary(mB)$r.squared, 4), "\n")

b_dist  <- coef(mB)["log_dist_nearest_km"]
b_inter <- coef(mB)["log_dist_nearest_km:nearest_reserve_cwb"]
cat(sprintf("\n  Marginal effect of distance at CWB Q1 (~45): %.3f\n", b_dist + b_inter*45))
cat(sprintf("  Marginal effect of distance at CWB Q4 (~75): %.3f\n", b_dist + b_inter*75))

# =============================================================================
# 6. MODEL C — Riding-level aggregated OLS (resolves power problem)
# =============================================================================
cat("\n=== MODEL C: Riding-level aggregated OLS (N=338 ridings) ===\n")

riding_means <- ces_analysis |>
  group_by(feduid) |>
  summarise(
    mean_therm       = mean(therm_indigenous, na.rm=TRUE),
    mean_resentment  = mean(resentment_index, na.rm=TRUE),
    mean_ideology    = mean(ideology_lr, na.rm=TRUE),
    mean_educ        = mean(educ_num, na.rm=TRUE),
    mean_age         = mean(age, na.rm=TRUE),
    pct_male         = mean(male, na.rm=TRUE),
    pct_born_canada  = mean(born_canada, na.rm=TRUE),
    n_respondents    = n(),
    .groups = "drop"
  ) |>
  left_join(prox |> mutate(FEDUID=as.numeric(FEDUID)) |>
              select(FEDUID, log_dist_nearest_km, nearest_reserve_cwb,
                     log_area_nonres_km2, area_nonres_km2,
                     dist_q_Q1_lowest_km, dist_q_Q4_highest_km),
            by = c("feduid"="FEDUID"))

cat("  Ridings with mean thermometer:", sum(!is.na(riding_means$mean_therm)), "\n")

mC <- lm(mean_therm ~
           log_dist_nearest_km * nearest_reserve_cwb +
           log_area_nonres_km2 +
           mean_ideology + mean_educ + mean_age + pct_male + pct_born_canada,
         data = riding_means |> filter(!is.na(mean_therm), !is.na(nearest_reserve_cwb)))

cat("  N ridings:", nobs(mC), "\n")
key_C <- c("log_dist_nearest_km","nearest_reserve_cwb",
           "log_dist_nearest_km:nearest_reserve_cwb","log_area_nonres_km2",
           "mean_ideology","mean_educ","mean_age","pct_male")
coef_C <- summary(mC)$coefficients
print(round(coef_C[key_C[key_C %in% rownames(coef_C)],], 4))
cat("  R²:", round(summary(mC)$r.squared, 4), "\n")

# =============================================================================
# 7. MODEL D — Resentment index, riding-level (robustness)
# =============================================================================
cat("\n=== MODEL D: Resentment index, riding-level (robustness) ===\n")
mD <- lm(mean_resentment ~
           log_dist_nearest_km * nearest_reserve_cwb +
           log_area_nonres_km2 +
           mean_ideology + mean_educ + mean_age + pct_male + pct_born_canada,
         data = riding_means |> filter(!is.na(mean_resentment), !is.na(nearest_reserve_cwb)))

cat("  N ridings:", nobs(mD), "\n")
coef_D <- summary(mD)$coefficients
print(round(coef_D[key_C[key_C %in% rownames(coef_D)],], 4))
cat("  R²:", round(summary(mD)$r.squared, 4), "\n")

# =============================================================================
# 8. Area tertile analysis: does the CWB effect vary by riding size?
# =============================================================================
cat("\n=== STRATIFIED BY RIDING AREA TERTILE (CWB effect by urbanicity) ===\n")

riding_means <- riding_means |>
  mutate(area_tertile = cut(log_area_nonres_km2,
                            breaks=quantile(log_area_nonres_km2, c(0,.33,.67,1), na.rm=TRUE),
                            labels=c("small_urban","medium","large_remote"),
                            include.lowest=TRUE))

for (tier in c("small_urban","medium","large_remote")) {
  sub <- riding_means |> filter(area_tertile == tier, !is.na(mean_therm), !is.na(nearest_reserve_cwb))
  if (nrow(sub) < 10) next
  m_tier <- lm(mean_therm ~ log_dist_nearest_km + nearest_reserve_cwb +
                 mean_ideology + mean_educ + mean_age + pct_male, data=sub)
  coef_tier <- summary(m_tier)$coefficients
  cat(sprintf("\n  %s (N=%d ridings):\n", tier, nobs(m_tier)))
  key_tier <- c("log_dist_nearest_km","nearest_reserve_cwb","mean_ideology")
  print(round(coef_tier[key_tier[key_tier %in% rownames(coef_tier)],], 4))
  cat(sprintf("  R²: %.4f\n", summary(m_tier)$r.squared))
}

# =============================================================================
# 9. Save
# =============================================================================
saveRDS(list(mA=mA, mB=mB, mC=mC, mD=mD), "data-clean/models_revised.rds")
saveRDS(riding_means, "data-clean/riding_means.rds")

cat("\n=== STEP 3 REVISED COMPLETE ===\n")
