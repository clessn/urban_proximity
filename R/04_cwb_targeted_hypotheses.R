# =============================================================================
# 04_cwb_targeted_hypotheses.R
# Step 4: Test CWB → policy attitudes (H1) and CWB → resentment (H2)
#
# H1: Nearest reserve CWB → reconciliation spending preference
#     (controlling for Indigenous thermometer to isolate policy reasoning from affect)
#
# H2: Nearest reserve CWB → resentment scale
#     (ab_favors + ab_deserve + ab_col: "do they perceive Indigenous people as privileged/needy?")
#
# Key: we want the direct effect of CWB on attitudes, NOT mediated by distance
#   → Include both log_dist_nearest_km AND nearest_reserve_cwb
#   → Control for thermometer in H1 (isolates cognition from affect)
#   → Cluster-robust SEs by federal riding throughout
# =============================================================================

library(haven)
library(dplyr)
library(readr)
library(sandwich)
library(lmtest)
library(MASS)   # for polr() ordinal logit
select <- dplyr::select  # resolve MASS::select conflict

setwd("/home/ral/.openclaw/workspace/urban_proximity")

# =============================================================================
# 1. Build analytical dataset
# =============================================================================
cat("Loading and merging data...\n")
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
    # Raw DVs not in ces_dv — pull directly from ces_full
    spend_raw   = as.numeric(cps21_spend_rec_indi),
    discrim_raw = as.numeric(cps21_groupdiscrim_1)
  )

stopifnot(all(ces_dv$feduid == controls$feduid))

df <- bind_cols(ces_dv, controls |> dplyr::select(-feduid)) |>
  filter(indigenous != 1 | is.na(indigenous)) |>      # exclude Indigenous respondents
  left_join(
    prox |>
      mutate(FEDUID = as.numeric(FEDUID)) |>
      dplyr::select(FEDUID, log_dist_nearest_km, nearest_reserve_cwb,
             log_area_nonres_km2),
    by = c("feduid" = "FEDUID")
  ) |>
  mutate(
    # ---- DV 1: Reconciliation spending ----
    # 1=spend less, 2=same, 3=spend more, 4=DK → NA
    spend_recode = case_when(
      spend_raw %in% 1:3 ~ spend_raw,
      TRUE ~ NA_real_
    ),
    spend_ordered = factor(spend_recode, levels=1:3,
                           labels=c("Less","Same","More"), ordered=TRUE),

    # ---- DV 2: Resentment scale (higher = more pro-Indigenous) ----
    # ab_favors_rev: reverse-coded (5 = strongly disagree with "no special favors")
    # ab_deserve:    agree = pro-Indigenous
    # ab_col:        agree = pro-Indigenous (colonialism explanation)
    resentment_index = rowMeans(
      cbind(ab_favors_rev, ab_deserve, ab_col), na.rm=FALSE
    ),

    # ---- DV 2b: perceived discrimination (groupdiscrim, reversed: 1=none → 5=great deal) ----
    discrim_perceived = case_when(
      discrim_raw %in% 1:5 ~ 6 - discrim_raw,  # reverse: higher = more discrimination perceived
      TRUE ~ NA_real_
    ),

    # ---- Thermometer (control in H1) ----
    therm = therm_indigenous,

    # ---- Province FE ----
    province = as.factor(as.numeric(cps21_province)),

    # CWB z-score (1 SD change = meaningful for tables)
    cwb_z = scale(nearest_reserve_cwb)[,1]
  )

cat("  N (non-Indigenous):", nrow(df), "\n")
cat("  N with spend DV:", sum(!is.na(df$spend_recode)), "\n")
cat("  N with resentment DV:", sum(!is.na(df$resentment_index)), "\n")
cat("  N with discrimination DV:", sum(!is.na(df$discrim_perceived)), "\n")


# =============================================================================
# 2. H1 — CWB → Reconciliation spending (OLS + ordinal logit)
#    Control for thermometer to isolate cognition from affect
# =============================================================================
cat("\n\n=============================================\n")
cat("H1: CWB of nearest reserve → Reconciliation spending\n")
cat("=============================================\n")

h1_base <- df |>
  filter(!is.na(spend_recode), !is.na(nearest_reserve_cwb),
         !is.na(log_dist_nearest_km), !is.na(ideology_lr),
         !is.na(educ_num), !is.na(age), !is.na(male))

cat("  N:", nrow(h1_base), "\n\n")

# H1-M1: CWB only (no thermometer control)
h1_m1 <- lm(spend_recode ~ nearest_reserve_cwb + log_dist_nearest_km +
               log_area_nonres_km2 + ideology_lr + educ_num +
               age + male + born_canada + province,
             data = h1_base)

h1_m1_rob <- coeftest(h1_m1, vcovCL(h1_m1, cluster=~feduid, data=h1_base))
cat("-- H1-M1: CWB → Spending (no thermometer control) --\n")
key <- c("nearest_reserve_cwb","log_dist_nearest_km","log_area_nonres_km2",
         "ideology_lr","educ_num","age","male")
print(round(h1_m1_rob[key[key %in% rownames(h1_m1_rob)],], 4))
cat(sprintf("R²=%.4f  N=%d\n\n", summary(h1_m1)$r.squared, nobs(h1_m1)))

# H1-M2: CWB + thermometer (key model — thermometer controls for affect)
h1_m2_data <- h1_base |> filter(!is.na(therm))
h1_m2 <- lm(spend_recode ~ nearest_reserve_cwb + log_dist_nearest_km +
               log_area_nonres_km2 + therm +
               ideology_lr + educ_num + age + male + born_canada + province,
             data = h1_m2_data)

h1_m2_rob <- coeftest(h1_m2, vcovCL(h1_m2, cluster=~feduid, data=h1_m2_data))
cat("-- H1-M2: CWB → Spending (controlling for thermometer) --\n")
key2 <- c("nearest_reserve_cwb","log_dist_nearest_km","log_area_nonres_km2",
          "therm","ideology_lr","educ_num","age","male")
print(round(h1_m2_rob[key2[key2 %in% rownames(h1_m2_rob)],], 4))
cat(sprintf("R²=%.4f  N=%d\n\n", summary(h1_m2)$r.squared, nobs(h1_m2)))

# H1-M3: Ordinal logit (proper model for 3-point ordered DV)
h1_m3_data <- h1_m2_data |> filter(!is.na(spend_ordered))
h1_m3 <- polr(spend_ordered ~ nearest_reserve_cwb + log_dist_nearest_km +
                log_area_nonres_km2 + therm +
                ideology_lr + educ_num + age + male + born_canada + province,
              data = h1_m3_data, Hess=TRUE)

cat("-- H1-M3: Ordinal logit (proper model) --\n")
coef_m3 <- summary(h1_m3)$coefficients
# Add p-values (polr doesn't compute them by default)
coef_m3_p <- cbind(coef_m3, p = 2*pnorm(abs(coef_m3[,"t value"]), lower.tail=FALSE))
key3 <- c("nearest_reserve_cwb","log_dist_nearest_km","log_area_nonres_km2",
          "therm","ideology_lr","educ_num","age","male","born_canada")
print(round(coef_m3_p[key3[key3 %in% rownames(coef_m3_p)],], 4))
cat(sprintf("N=%d\n\n", nobs(h1_m3)))


# =============================================================================
# 3. H2 — CWB → Resentment scale
#    (higher resentment = less sympathy, "special favors" framing)
# =============================================================================
cat("=============================================\n")
cat("H2: CWB of nearest reserve → Resentment / Perceived privilege\n")
cat("=============================================\n")

h2_base <- df |>
  filter(!is.na(resentment_index), !is.na(nearest_reserve_cwb),
         !is.na(log_dist_nearest_km), !is.na(ideology_lr),
         !is.na(educ_num), !is.na(age), !is.na(male))

cat("  N:", nrow(h2_base), "\n\n")

# H2-M1: CWB → Resentment (no thermometer — resentment IS the cognition)
h2_m1 <- lm(resentment_index ~ nearest_reserve_cwb + log_dist_nearest_km +
               log_area_nonres_km2 + ideology_lr + educ_num +
               age + male + born_canada + province,
             data = h2_base)

h2_m1_rob <- coeftest(h2_m1, vcovCL(h2_m1, cluster=~feduid, data=h2_base))
cat("-- H2-M1: CWB → Resentment Index (higher=more pro-Indigenous) --\n")
print(round(h2_m1_rob[key[key %in% rownames(h2_m1_rob)],], 4))
cat(sprintf("R²=%.4f  N=%d\n\n", summary(h2_m1)$r.squared, nobs(h2_m1)))

# H2-M2: Each resentment item separately
cat("-- H2-M2: CWB → each resentment item separately --\n")
items <- list(
  ab_favors_rev = "\"Should work up without special favors\" (reversed: higher=disagree)",
  ab_deserve    = "\"Got less than they deserve\" (higher=agree)",
  ab_col        = "\"Colonialism created conditions\" (higher=agree)"
)
for (item_name in names(items)) {
  item_df <- df |> filter(!is.na(.data[[item_name]]), !is.na(nearest_reserve_cwb),
                           !is.na(log_dist_nearest_km), !is.na(ideology_lr),
                           !is.na(educ_num), !is.na(age), !is.na(male))
  m_item <- lm(as.formula(paste(item_name, "~ nearest_reserve_cwb + log_dist_nearest_km +
                 log_area_nonres_km2 + ideology_lr + educ_num + age + male + born_canada + province")),
               data = item_df)
  m_rob <- coeftest(m_item, vcovCL(m_item, cluster=~feduid, data=item_df))
  cwb_row <- m_rob["nearest_reserve_cwb",]
  dist_row <- m_rob["log_dist_nearest_km",]
  cat(sprintf("  %s\n  %s\n", item_name, items[[item_name]]))
  cat(sprintf("  CWB β=%.4f SE=%.4f t=%.3f p=%.4f | Dist β=%.4f p=%.4f | N=%d\n\n",
              cwb_row[1], cwb_row[2], cwb_row[3], cwb_row[4],
              dist_row[1], dist_row[4], nobs(m_item)))
}

# H2-M3: CWB → Perceived discrimination (groupdiscrim, split-sample)
h2_m3_data <- df |>
  filter(!is.na(discrim_perceived), !is.na(nearest_reserve_cwb),
         !is.na(log_dist_nearest_km), !is.na(ideology_lr),
         !is.na(educ_num), !is.na(age), !is.na(male))

h2_m3 <- lm(discrim_perceived ~ nearest_reserve_cwb + log_dist_nearest_km +
               log_area_nonres_km2 + ideology_lr + educ_num +
               age + male + born_canada + province,
             data = h2_m3_data)

h2_m3_rob <- coeftest(h2_m3, vcovCL(h2_m3, cluster=~feduid, data=h2_m3_data))
cat("-- H2-M3: CWB → Perceived discrimination against Indigenous (higher=more perceived) --\n")
cat(sprintf("  Split-sample N=%d (27%% of CPS respondents)\n", nobs(h2_m3)))
print(round(h2_m3_rob[key[key %in% rownames(h2_m3_rob)],], 4))
cat(sprintf("R²=%.4f\n\n", summary(h2_m3)$r.squared))


# =============================================================================
# 4. Summary table
# =============================================================================
cat("\n=============================================\n")
cat("SUMMARY: CWB coefficient across all models\n")
cat("(cluster-robust SEs)\n")
cat("=============================================\n")

results <- rbind(
  c("H1-M1", "Reconciliation spending (OLS)", "No affect ctrl",
    round(h1_m1_rob["nearest_reserve_cwb",1],4),
    round(h1_m1_rob["nearest_reserve_cwb",2],4),
    round(h1_m1_rob["nearest_reserve_cwb",4],4),
    nobs(h1_m1)),
  c("H1-M2", "Reconciliation spending (OLS)", "+ thermometer ctrl",
    round(h1_m2_rob["nearest_reserve_cwb",1],4),
    round(h1_m2_rob["nearest_reserve_cwb",2],4),
    round(h1_m2_rob["nearest_reserve_cwb",4],4),
    nobs(h1_m2)),
  c("H2-M1", "Resentment index (OLS)", "Higher=pro-Indigenous",
    round(h2_m1_rob["nearest_reserve_cwb",1],4),
    round(h2_m1_rob["nearest_reserve_cwb",2],4),
    round(h2_m1_rob["nearest_reserve_cwb",4],4),
    nobs(h2_m1)),
  c("H2-M3", "Perceived discrimination (OLS)", "Higher=more perceived",
    round(h2_m3_rob["nearest_reserve_cwb",1],4),
    round(h2_m3_rob["nearest_reserve_cwb",2],4),
    round(h2_m3_rob["nearest_reserve_cwb",4],4),
    nobs(h2_m3))
)
colnames(results) <- c("Model","DV","Note","CWB β","SE","p","N")
print(as.data.frame(results))

