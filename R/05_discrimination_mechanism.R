# =============================================================================
# 05_discrimination_mechanism.R
# Step 5: Deep-dive on the core mechanism
#
# Theory: Proximity to WEALTHY reserves → perceive less discrimination
#   against Indigenous peoples → "they're doing fine, stop complaining"
#
# Key DV: cps21_groupdiscrim_1
#   "How much discrimination is there in Canada against Indigenous peoples?"
#   1=A great deal → 5=None at all (we keep original direction, higher=more discrimination)
#
# Key IVs:
#   - nearest_reserve_cwb (CWB score of nearest reserve)
#   - log_dist_nearest_km (distance to nearest reserve)
#   - Interaction: dist × CWB
#
# Also: verify Kahnawake (QC) / Wendake (QC) as nearest for Montreal/Quebec City
# =============================================================================

library(haven)
library(dplyr)
library(readr)
library(sandwich)
library(lmtest)
select <- dplyr::select

setwd("/home/ral/.openclaw/workspace/urban_proximity")

# =============================================================================
# 1. Build dataset
# =============================================================================
cat("Loading data...\n")
ces_full <- readRDS("data/ces2021.rds")
ces_dv   <- readRDS("data-clean/ces2021_dv.rds")
prox     <- read_csv("data-clean/proximity_fed_corrected.csv", show_col_types=FALSE)

controls <- ces_full |>
  transmute(
    feduid,
    age         = ifelse(as.numeric(cps21_age) >= 18 & as.numeric(cps21_age) <= 100,
                         as.numeric(cps21_age), NA),
    educ_num    = ifelse(as.numeric(cps21_education) %in% 1:11,
                         as.numeric(cps21_education), NA),
    male        = ifelse(as.numeric(cps21_genderid) == 1, 1L, 0L),
    ideology_lr = ifelse(as.numeric(cps21_lr_scale_bef_1) >= 0 &
                           as.numeric(cps21_lr_scale_bef_1) <= 10,
                         as.numeric(cps21_lr_scale_bef_1), NA),
    born_canada = ifelse(as.numeric(cps21_bornin_canada) == 1, 1L, 0L),
    indigenous  = ifelse(as.numeric(cps21_vismin_4) == 1, 1L, 0L),
    # Raw DV
    discrim_raw    = as.numeric(cps21_groupdiscrim_1),
    # Province
    prov_raw       = as.numeric(cps21_province)
  )

stopifnot(all(ces_dv$feduid == controls$feduid))

df <- bind_cols(ces_dv, controls |> select(-feduid)) |>
  filter(indigenous != 1 | is.na(indigenous)) |>
  left_join(
    prox |>
      mutate(FEDUID = as.numeric(FEDUID)) |>
      select(FEDUID, log_dist_nearest_km, dist_nearest_reserve_km,
             nearest_reserve_cwb, nearest_reserve_name, nearest_reserve_uid,
             nearest_reserve_pop, log_area_nonres_km2, area_nonres_km2),
    by = c("feduid" = "FEDUID")
  ) |>
  mutate(
    province = as.factor(prov_raw),

    # DV: perceived discrimination, ORIGINAL direction
    # 1=A great deal, 2=A lot, 3=Moderate, 4=A little, 5=None at all, 6=DK→NA
    discrim_hi_to_lo = case_when(
      discrim_raw %in% 1:5 ~ discrim_raw,
      TRUE ~ NA_real_
    ),
    # Recoded: HIGHER = MORE discrimination perceived (reverse of scale)
    discrim_perceived = case_when(
      discrim_raw %in% 1:5 ~ 6 - discrim_raw,
      TRUE ~ NA_real_
    ),
    # Binary: perceives "a great deal" or "a lot" vs less
    discrim_high = ifelse(discrim_raw %in% 1:2, 1L, 0L),

    # CWB z-score for standardized interpretation
    cwb_z = as.numeric(scale(nearest_reserve_cwb)),
    dist_z = as.numeric(scale(log_dist_nearest_km)),

    # Reserve wealth tertile
    cwb_tertile = cut(nearest_reserve_cwb,
                      breaks = quantile(nearest_reserve_cwb, c(0,.33,.67,1), na.rm=TRUE),
                      labels = c("poor","middle","wealthy"),
                      include.lowest = TRUE)
  )

cat("  N (non-Indigenous):", nrow(df), "\n")
cat("  N with discrim DV:", sum(!is.na(df$discrim_perceived)), "\n")
cat("  Split-sample: ~27% of respondents got this question\n\n")

# =============================================================================
# 2. Verify the Kahnawake / Wendake story
# =============================================================================
cat("=== RESERVE GEOGRAPHY CHECK: Quebec ===\n")

# What is the nearest reserve for Montreal / Quebec City ridings?
qc_ridings <- prox |>
  mutate(FEDUID = as.numeric(FEDUID)) |>
  filter(FEDUID %in% c(24033, 24042, 24055, 24064, 24012, 24015, 24019, 24027, 24039)) |>
  # Montreal/QC City riding UIDs roughly in the 24xxx range
  select(FEDUID, nearest_reserve_name, nearest_reserve_cwb, dist_nearest_reserve_km) |>
  arrange(dist_nearest_reserve_km)

cat("Nearest reserves for selected Quebec ridings:\n")
print(qc_ridings)

# Full QC riding picture
cat("\nAll QC ridings — nearest reserve and CWB:\n")
qc_all <- prox |>
  mutate(FEDUID = as.numeric(FEDUID)) |>
  filter(floor(FEDUID/1000) == 24) |>
  select(FEDUID, nearest_reserve_name, nearest_reserve_cwb, dist_nearest_reserve_km) |>
  arrange(nearest_reserve_cwb)
print(qc_all)

# Distribution of nearest reserve CWB for QC vs ON vs BC
cat("\nMean CWB of nearest reserve by province (individual-level):\n")
prov_cwb <- df |>
  filter(!is.na(nearest_reserve_cwb)) |>
  group_by(prov_raw) |>
  summarise(
    mean_cwb_nearest = round(mean(nearest_reserve_cwb, na.rm=TRUE), 1),
    median_cwb_nearest = round(median(nearest_reserve_cwb, na.rm=TRUE), 1),
    mean_dist_km = round(mean(dist_nearest_reserve_km, na.rm=TRUE), 1),
    n = n(),
    .groups = "drop"
  )
print(prov_cwb)

# =============================================================================
# 3. Core model: CWB → Perceived discrimination
# =============================================================================
cat("\n\n=== CORE MODEL: CWB → Perceived Discrimination ===\n")
cat("DV: How much discrimination against Indigenous peoples?\n")
cat("(Higher = MORE discrimination perceived; 1=none at all → 5=a great deal)\n\n")

m_base <- df |>
  filter(!is.na(discrim_perceived), !is.na(nearest_reserve_cwb),
         !is.na(log_dist_nearest_km), !is.na(ideology_lr),
         !is.na(educ_num), !is.na(age), !is.na(male))

cat("N:", nrow(m_base), "\n\n")

# M1: CWB only (no distance)
m1 <- lm(discrim_perceived ~ nearest_reserve_cwb +
            log_area_nonres_km2 + ideology_lr + educ_num +
            age + male + born_canada + province,
          data = m_base)
m1_rob <- coeftest(m1, vcovCL(m1, cluster=~feduid, data=m_base))
cat("-- M1: CWB → Discrimination (no distance) --\n")
key <- c("nearest_reserve_cwb","log_area_nonres_km2","ideology_lr","educ_num","age","male","born_canada")
print(round(m1_rob[key[key %in% rownames(m1_rob)],], 4))
cat(sprintf("R²=%.4f  N=%d\n\n", summary(m1)$r.squared, nobs(m1)))

# M2: CWB + distance (both together)
m2 <- lm(discrim_perceived ~ nearest_reserve_cwb + log_dist_nearest_km +
            log_area_nonres_km2 + ideology_lr + educ_num +
            age + male + born_canada + province,
          data = m_base)
m2_rob <- coeftest(m2, vcovCL(m2, cluster=~feduid, data=m_base))
cat("-- M2: CWB + distance (both) --\n")
key2 <- c("nearest_reserve_cwb","log_dist_nearest_km","log_area_nonres_km2",
          "ideology_lr","educ_num","age","male","born_canada")
print(round(m2_rob[key2[key2 %in% rownames(m2_rob)],], 4))
cat(sprintf("R²=%.4f  N=%d\n\n", summary(m2)$r.squared, nobs(m2)))

# M3: CWB × distance interaction — does the CWB effect strengthen when you're CLOSE?
m3 <- lm(discrim_perceived ~ nearest_reserve_cwb * log_dist_nearest_km +
            log_area_nonres_km2 + ideology_lr + educ_num +
            age + male + born_canada + province,
          data = m_base)
m3_rob <- coeftest(m3, vcovCL(m3, cluster=~feduid, data=m_base))
cat("-- M3: CWB × Distance interaction --\n")
key3 <- c("nearest_reserve_cwb","log_dist_nearest_km",
          "nearest_reserve_cwb:log_dist_nearest_km",
          "log_area_nonres_km2","ideology_lr","educ_num","age","male")
print(round(m3_rob[key3[key3 %in% rownames(m3_rob)],], 4))
cat(sprintf("R²=%.4f  N=%d\n\n", summary(m3)$r.squared, nobs(m3)))

# Marginal effect of CWB at different distances
b_cwb   <- coef(m3)["nearest_reserve_cwb"]
b_inter <- coef(m3)["nearest_reserve_cwb:log_dist_nearest_km"]
cat("  Marginal effect of CWB (1 unit) at:\n")
for (d_km in c(5, 20, 50, 100, 300)) {
  me <- b_cwb + b_inter * log1p(d_km)
  cat(sprintf("    %4d km: %.5f\n", d_km, me))
}

# =============================================================================
# 4. Standardized coefficients (for effect size comparison)
# =============================================================================
cat("\n-- M2 standardized (z-scores, for effect size) --\n")
m_base_z <- m_base |>
  mutate(
    discrim_z = as.numeric(scale(discrim_perceived)),
    cwb_z     = as.numeric(scale(nearest_reserve_cwb)),
    dist_z    = as.numeric(scale(log_dist_nearest_km)),
    area_z    = as.numeric(scale(log_area_nonres_km2)),
    ideol_z   = as.numeric(scale(ideology_lr)),
    educ_z    = as.numeric(scale(educ_num)),
    age_z     = as.numeric(scale(age))
  )
m2_std <- lm(discrim_z ~ cwb_z + dist_z + area_z + ideol_z + educ_z +
               age_z + male + born_canada + province,
             data = m_base_z)
m2_std_rob <- coeftest(m2_std, vcovCL(m2_std, cluster=~feduid, data=m_base_z))
std_key <- c("cwb_z","dist_z","area_z","ideol_z","educ_z","age_z","male","born_canada")
print(round(m2_std_rob[std_key[std_key %in% rownames(m2_std_rob)],], 4))

# =============================================================================
# 5. Stratify by province — is the QC effect stronger?
# =============================================================================
cat("\n\n=== PROVINCIAL STRATIFICATION ===\n")
cat("Does the CWB effect operate differently in QC (Kahnawake/Wendake) vs others?\n\n")

prov_labels <- c("1"="NL","2"="PEI","3"="NS","4"="NB","5"="QC","6"="ON",
                 "7"="MB","8"="SK","9"="AB","10"="BC","11"="YT","12"="NWT","13"="NU")
# Recode province
df <- df |> mutate(prov_name = recode(as.character(prov_raw), !!!prov_labels))

for (prov in c("QC","ON","BC","AB","MB")) {
  prov_num <- as.integer(names(prov_labels)[prov_labels == prov])
  sub <- m_base |> filter(prov_raw == prov_num)
  if (nrow(sub) < 30) {
    cat(sprintf("  %s: N=%d (too small)\n", prov, nrow(sub)))
    next
  }
  m_prov <- lm(discrim_perceived ~ nearest_reserve_cwb + log_dist_nearest_km +
                 log_area_nonres_km2 + ideology_lr + educ_num + age + male,
               data = sub)
  # Safe extraction with tryCatch
  m_prov_rob <- tryCatch(
    coeftest(m_prov, vcovCL(m_prov, cluster=~feduid, data=sub)),
    error = function(e) summary(m_prov)$coefficients
  )
  cwb_β  <- if ("nearest_reserve_cwb"   %in% rownames(m_prov_rob)) m_prov_rob["nearest_reserve_cwb", ] else c(NA,NA,NA,NA)
  dist_β <- if ("log_dist_nearest_km"   %in% rownames(m_prov_rob)) m_prov_rob["log_dist_nearest_km", ] else c(NA,NA,NA,NA)
  cat(sprintf("  %s (N=%d): CWB β=%.4f p=%.3f | Dist β=%.4f p=%.3f | R²=%.3f\n",
              prov, nrow(sub),
              cwb_β[1], cwb_β[4],
              dist_β[1], dist_β[4],
              summary(m_prov)$r.squared))
}

# =============================================================================
# 6. CWB tertile means — descriptive table
# =============================================================================
cat("\n\n=== DESCRIPTIVE: Mean perceived discrimination by CWB tertile of nearest reserve ===\n")
desc <- df |>
  filter(!is.na(discrim_perceived), !is.na(cwb_tertile)) |>
  group_by(cwb_tertile) |>
  summarise(
    n = n(),
    mean_discrim  = round(mean(discrim_perceived, na.rm=TRUE), 3),
    sd_discrim    = round(sd(discrim_perceived, na.rm=TRUE), 3),
    pct_high_discrim = round(100*mean(discrim_high, na.rm=TRUE), 1),
    mean_cwb      = round(mean(nearest_reserve_cwb, na.rm=TRUE), 1),
    mean_dist_km  = round(mean(dist_nearest_reserve_km, na.rm=TRUE), 1),
    .groups="drop"
  )
cat("(discrim scale: 1=none at all, 5=a great deal)\n")
print(desc)

cat("\nRaw difference (poor tertile - wealthy tertile):",
    round(desc$mean_discrim[desc$cwb_tertile=="poor"] -
          desc$mean_discrim[desc$cwb_tertile=="wealthy"], 3), "\n")

