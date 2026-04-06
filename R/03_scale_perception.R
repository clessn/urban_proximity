# =============================================================================
# 03_scale_perception.R
# Step 3a: Build and validate "Perception of Indigenous Peoples" scale
#
# SCALE CONVENTION: Higher score = MORE NEGATIVE perception / MORE resentment
#
# Items included (perception/affect only — policy items excluded):
#   cps21_groups_therm_4   Feeling thermometer 0-100 (100=like → reversed)
#   cps21_groupdiscrim_1   Perceived discrimination 1=great deal → 5=none
#   pes21_ab_favors        "No special favors" resentment (1=SD → 5=SA)
#   pes21_ab_deserve       "Got less than they deserve" sympathy (1=SD → 5=SA) → reversed
#   pes21_ab_col           Structural/colonial attribution (1=SD → 5=SA) → reversed
#
# Directionality:
#   therm_indigenous_rev   = (100 - therm_raw) / 100 * 5   → [0-5], higher=negative
#   groupdiscrim_neg       = cps21_groupdiscrim_1 as-is    → [1-5], higher=negative (less perceived discrim)
#   ab_favors_neg          = pes21_ab_favors as-is          → [1-5], higher=negative (resentment)
#   ab_deserve_neg         = 6 - pes21_ab_deserve           → [1-5], higher=negative (denies they deserve less)
#   ab_col_neg             = 6 - pes21_ab_col               → [1-5], higher=negative (denies structural causes)
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

# devtools::install_github("clessn/sondr")
library(sondr)
library(dplyr)
library(tidyr)
library(haven)

setwd(here::here())


# =============================================================================
# 1. LOAD DATA
# =============================================================================

ces <- readRDS("data/ces2021.rds")
cat("CES 2021 loaded:", nrow(ces), "respondents x", ncol(ces), "variables\n")


# =============================================================================
# 2. RECODE ITEMS — ALL IN "HIGHER = MORE NEGATIVE" DIRECTION
# =============================================================================

ces <- ces %>%
  mutate(

    # --- Feeling thermometer ---
    # Raw: 0-100 (0=dislike, 100=like), -99 = DK/NA
    # Action: recode DK to NA, then reverse and rescale to [0-5]
    therm_raw = ifelse(as.numeric(cps21_groups_therm_4) == -99, NA,
                       as.numeric(cps21_groups_therm_4)),
    therm_neg = (100 - therm_raw) / 20,   # rescale to [0, 5]; higher = more negative

    # --- Perceived discrimination ---
    # Raw: 1=A great deal, 5=None at all, 6=DK
    # Direction: as-is — higher already means LESS perceived discrimination = more negative
    groupdiscrim_neg = case_when(
      as.numeric(cps21_groupdiscrim_1) %in% 1:5 ~ as.numeric(cps21_groupdiscrim_1),
      TRUE ~ NA_real_
    ),

    # --- "No special favors" (resentment) ---
    # Raw: 1=SD, 5=SA, 6=DK
    # Direction: higher = more resentment = more negative perception → keep as-is
    ab_favors_neg = case_when(
      as.numeric(pes21_ab_favors) %in% 1:5 ~ as.numeric(pes21_ab_favors),
      TRUE ~ NA_real_
    ),

    # --- "Got less than they deserve" (sympathy) ---
    # Raw: 1=SD, 5=SA, 6=DK
    # Direction: SA means MORE sympathy = more positive perception → REVERSE
    ab_deserve_neg = case_when(
      as.numeric(pes21_ab_deserve) %in% 1:5 ~ 6 - as.numeric(pes21_ab_deserve),
      TRUE ~ NA_real_
    ),

    # --- Structural attribution (colonialism/discrimination) ---
    # Raw: 1=SD, 5=SA, 6=DK
    # Direction: SA means endorsing structural cause = more positive/sympathetic → REVERSE
    ab_col_neg = case_when(
      as.numeric(pes21_ab_col) %in% 1:5 ~ 6 - as.numeric(pes21_ab_col),
      TRUE ~ NA_real_
    )

  )

# Quick sanity check: distributions
cat("\n=== RECODED ITEMS (higher = more negative perception) ===\n")
cat("\ntherm_neg [0-5]:\n"); summary(ces$therm_neg)
cat("\ngroupdiscrim_neg [1-5]:\n"); print(table(ces$groupdiscrim_neg, useNA = "ifany"))
cat("\nab_favors_neg [1-5]:\n");   print(table(ces$ab_favors_neg, useNA = "ifany"))
cat("\nab_deserve_neg [1-5]:\n");  print(table(ces$ab_deserve_neg, useNA = "ifany"))
cat("\nab_col_neg [1-5]:\n");      print(table(ces$ab_col_neg, useNA = "ifany"))


# =============================================================================
# 3. FACTOR ANALYSIS — sondr::topdown_fa()
# =============================================================================
# topdown_fa() expects a dataframe of items only.
# It internally calls drop_na() so missingness is handled automatically.
# We pass all 5 items; the function will report factor loadings and eigenvalues.
#
# NOTE on split-sample:
#   - therm_neg and groupdiscrim_neg are CPS items (~19k and ~5.8k resp.)
#   - ab_* items are PES items (~14.5k resp.)
#   - drop_na() will reduce N to respondents with all 5 items (~5k–6k)
#   - This is expected and acceptable for FA purposes

scale_items <- ces %>%
  select(
    therm_neg,
    groupdiscrim_neg,
    ab_favors_neg,
    ab_deserve_neg,
    ab_col_neg
  )

cat("\n=== N by item coverage ===\n")
cat("therm_neg valid:", sum(!is.na(ces$therm_neg)), "\n")
cat("groupdiscrim_neg valid:", sum(!is.na(ces$groupdiscrim_neg)), "\n")
cat("ab_favors_neg valid:", sum(!is.na(ces$ab_favors_neg)), "\n")
cat("ab_deserve_neg valid:", sum(!is.na(ces$ab_deserve_neg)), "\n")
cat("ab_col_neg valid:", sum(!is.na(ces$ab_col_neg)), "\n")
cat("All 5 items non-missing:", sum(complete.cases(scale_items)), "\n")

cat("\n=== RUNNING FACTOR ANALYSIS (sondr::topdown_fa) ===\n")
fa_result <- sondr::topdown_fa(scale_items)
print(fa_result)


# =============================================================================
# 4. INTERPRETATION GUIDE (printed after FA output)
# =============================================================================

cat("
=== INTERPRETATION GUIDE ===

All items recoded so higher = more negative perception of Indigenous peoples.

Expected outcomes:
  A) 1 factor: attitudes are unidimensional → use factor score directly as DV
  B) 2 factors: likely split between...
     Factor 1: AFFECT (therm_neg + groupdiscrim_neg)
     Factor 2: RESENTMENT (ab_favors_neg + ab_deserve_neg + ab_col_neg)
     → Use Factor 1 or Factor 2 based on theoretical motivation.
     → Contact theory predicts affect (Factor 1) should respond to proximity.

Next steps:
  - If unidimensional: save factor scores as dv_perception
  - If 2 factors: save separately as dv_affect and dv_resentment
  - Then run: 04_merge_proximity.R
")


# =============================================================================
# 5. SAVE RECODED ITEMS TO CLEAN DATA FILE
# =============================================================================

ces_scale <- ces %>%
  select(
    feduid, fedname,
    cps21_province,
    cps21_vismin_4,
    therm_neg,
    groupdiscrim_neg,
    ab_favors_neg,
    ab_deserve_neg,
    ab_col_neg
  )

dir.create("data-clean", showWarnings = FALSE)
saveRDS(ces_scale, "data-clean/ces2021_scale_items.rds")
cat("\nSaved recoded scale items: data-clean/ces2021_scale_items.rds\n")
cat("N rows:", nrow(ces_scale), "\n")
