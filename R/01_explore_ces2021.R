# =============================================================================
# 01_explore_ces2021.R
# Step 1: Explore CES 2021 — identify DV candidates (Indigenous attitudes)
#         and geographic identifiers for spatial analysis
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data -------------------------------------------------------------------
ces <- readRDS("data/ces2021.rds")
cat("CES 2021 loaded:", nrow(ces), "respondents x", ncol(ces), "variables\n")


# =============================================================================
# SECTION 1: DEPENDENT VARIABLE CANDIDATES
# =============================================================================

# --- 1a. Feeling thermometer (0-100 continuous) ---
# cps21_groups_therm_4: "How do you feel about Indigenous peoples?" (0-100)
# -99 = Don't know/Prefer not to answer
ces <- ces %>%
  mutate(
    therm_indigenous = ifelse(as.numeric(cps21_groups_therm_4) == -99, NA,
                              as.numeric(cps21_groups_therm_4))
  )
cat("\nThermometer (0-100):\n")
summary(ces$therm_indigenous)
# N valid
cat("N valid:", sum(!is.na(ces$therm_indigenous)), "\n")


# --- 1b. Reconciliation spending (3-point) ---
# cps21_spend_rec_indi: 1=Spend less, 2=Same, 3=Spend more, 4=DK/NA
ces <- ces %>%
  mutate(
    spend_reconciliation = case_when(
      as.numeric(cps21_spend_rec_indi) %in% 1:3 ~ as.numeric(cps21_spend_rec_indi),
      TRUE ~ NA_real_
    )
  )
cat("\nReconciliation spending (1=less, 2=same, 3=more):\n")
print(table(ces$spend_reconciliation, useNA = "ifany"))


# --- 1c. Perceived discrimination against Indigenous peoples ---
# cps21_groupdiscrim_1: 1=A great deal, 5=None at all, 6=DK
# NOTE: Only 27.5% of respondents answered this (split-sample)
ces <- ces %>%
  mutate(
    discrimination_perceived = case_when(
      as.numeric(cps21_groupdiscrim_1) %in% 1:5 ~ as.numeric(cps21_groupdiscrim_1),
      TRUE ~ NA_real_
    )
  )
cat("\nPerceived discrimination (1=great deal, 5=none, split-sample n=5,775):\n")
print(table(ces$discrimination_perceived, useNA = "ifany"))


# --- 1d. Residential school / reconciliation policy support ---
# 4 items, 4-point scale (1=strongly oppose, 4=strongly support)
resi_vars <- c("cps21_residential_2a", "cps21_residential_2b",
               "cps21_residential_2c", "cps21_residential_2d")
resi_labels <- c("TRC calls to action", "Unmarked graves funding",
                  "Cease court actions", "Rename buildings")
# NOTE: also split-sample (27.5% of respondents)
for (i in seq_along(resi_vars)) {
  ces[[paste0("resi_", i)]] <- ifelse(
    as.numeric(ces[[resi_vars[i]]]) %in% 1:4,
    as.numeric(ces[[resi_vars[i]]]),
    NA
  )
  cat(sprintf("\n%s [%s]:\n", resi_labels[i], resi_vars[i]))
  print(table(ces[[paste0("resi_", i)]], useNA = "ifany"))
}


# --- 1e. Indigenous Resentment Scale (PES) ---
# pes21_ab_favors: "should work their way up without special favors" (1=SD, 5=SA)
# pes21_ab_deserve: "have gotten less than they deserve" (1=SD, 5=SA)
# pes21_ab_col: "colonialism created difficult conditions" (1=SD, 5=SA)
# NOTE: 71.9% coverage (PES respondents)

# Recode: higher = more favorable / pro-Indigenous
ces <- ces %>%
  mutate(
    # Reverse-code: agree with "special favors" argument = NEGATIVE attitude
    ab_favors_rev = case_when(
      as.numeric(pes21_ab_favors) %in% 1:5 ~ 6 - as.numeric(pes21_ab_favors),
      TRUE ~ NA_real_
    ),
    # Keep direction: agree = more favorable
    ab_deserve = case_when(
      as.numeric(pes21_ab_deserve) %in% 1:5 ~ as.numeric(pes21_ab_deserve),
      TRUE ~ NA_real_
    ),
    ab_col = case_when(
      as.numeric(pes21_ab_col) %in% 1:5 ~ as.numeric(pes21_ab_col),
      TRUE ~ NA_real_
    )
  )

cat("\nResentment scale items (higher = more pro-Indigenous):\n")
cat("ab_favors_rev (5=strongly DISAGREE with 'no special favors' argument):\n")
print(table(ces$ab_favors_rev, useNA = "ifany"))
cat("ab_deserve (5=strongly agree they got less than they deserve):\n")
print(table(ces$ab_deserve, useNA = "ifany"))
cat("ab_col (5=strongly agree colonialism explains conditions):\n")
print(table(ces$ab_col, useNA = "ifany"))

# Cronbach's alpha check for resentment scale
cat("\nCorrelation between resentment items (Pearson):\n")
print(cor(ces[, c("ab_favors_rev", "ab_deserve", "ab_col")], use = "complete.obs"))


# =============================================================================
# SECTION 2: GEOGRAPHIC IDENTIFIERS
# =============================================================================

cat("\n\n=== GEOGRAPHIC RESOLUTION SUMMARY ===\n")

# Federal Electoral District (FED) — 338 districts
cat("\nfeduid (Federal Electoral District unique ID):\n")
cat("  Coverage:", sum(!is.na(ces$feduid)), "/", nrow(ces), "\n")
cat("  N unique ridings:", length(unique(na.omit(ces$feduid))), "\n")
cat("  Strategy: Join to Statistics Canada FED shapefile\n")
cat("  Then: Compute centroid of each FED → distance to nearest reserve centroid\n")
cat("  Precision: ±20-50 km (district centroids, not exact addresses)\n")

# Province — 13 provinces/territories
cat("\nProvince:\n")
cat("  Coverage: 100%\n")
print(table(as_factor(ces$cps21_province)))

# Self-reported urban/rural
cat("\nSelf-reported urbanicity (pes21_rural_urban, PES only):\n")
print(table(as_factor(ces$pes21_rural_urban), useNA = "ifany"))

# Indigenous identity (for exclusion analysis)
cat("\nN respondents identifying as Indigenous (cps21_vismin_4):\n")
cat("  Indigenous:", sum(as.numeric(ces$cps21_vismin_4) == 1, na.rm = TRUE), "\n")
cat("  Non-Indigenous:", sum(as.numeric(ces$cps21_vismin_4) != 1, na.rm = TRUE), "\n")


# =============================================================================
# SECTION 3: RECOMMENDED DV STRATEGY
# =============================================================================

cat("\n\n=== RECOMMENDED DV STRATEGY ===\n")
cat("
PRIMARY DV (full sample, N=~19,390 non-Indigenous respondents):
  → cps21_groups_therm_4 (feeling thermometer, 0-100)
  Advantages: Continuous, full CPS sample, well-understood, widely used in CES lit
  Recoding: -99 → NA; otherwise keep raw score

SECONDARY DV #1 (PES sample, N=~10,800):
  → Indigenous Resentment Index = mean(ab_favors_rev, ab_deserve, ab_col)
  Advantages: Multi-item scale (more reliable), captures policy attitudes vs affect
  Note: Requires PES respondents only; lower N but stronger construct validity

SECONDARY DV #2 (split-sample, N=~5,200):
  → Reconciliation spending (spend_reconciliation, 1-3)
  Advantages: Clear policy preference; ordinal logit appropriate
  Disadvantage: Only 1/4 of respondents; lower power for interaction analysis

GEOGRAPHIC STRATEGY:
  → Use feduid (338 federal ridings) as primary spatial unit
  → Join to Statistics Canada FED shapefile (available free)
  → Compute FED centroid → distance to nearest Indian Reserve centroid
  → Classify reserves by CWB index and rural/urban status
  → Precision: ±20-50 km (good enough for population-level variation)
  → Sensitivity check: postal FSA linkage (requires PCCF — available via CHASS)

SAMPLE RESTRICTIONS:
  → Exclude self-identified Indigenous respondents (n=633) from main analysis
  → Run separately as robustness check
")


# =============================================================================
# SECTION 4: SAVE CLEAN DV DATASET
# =============================================================================

ces_clean <- ces %>%
  select(
    # IDs
    cps21_StartDate, RecordedDate,
    # Geographic
    cps21_province, pes21_province, Region, feduid, fedname,
    provcode, pes21_rural_urban,
    # PCCF flags
    pccf_pcode_problem, manual_PCCF,
    # Indigenous identity
    cps21_vismin_4,
    # DV: feeling thermometer
    therm_indigenous,
    # DV: reconciliation spending
    spend_reconciliation,
    # DV: perceived discrimination
    discrimination_perceived,
    # DV: residential school policy
    resi_1, resi_2, resi_3, resi_4,
    # DV: resentment scale
    ab_favors_rev, ab_deserve, ab_col,
    # Key controls (to be expanded in 02_controls.R)
    cps21_province
  )

dir.create("data-clean", showWarnings = FALSE)
saveRDS(ces_clean, "data-clean/ces2021_dv.rds")
cat("Saved clean DV dataset:", nrow(ces_clean), "x", ncol(ces_clean), "\n")
cat("Path: data-clean/ces2021_dv.rds\n")
