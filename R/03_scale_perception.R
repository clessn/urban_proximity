# =============================================================================
# 03_scale_perception.R
# Step 3a: Build and validate "Perception of Indigenous Peoples" scale
#
# CONVENTION: Higher score = MORE NEGATIVE perception / MORE resentment
# ALL items rescaled to [0, 1] after directionality correction
#
# Items:
#   cps21_groups_therm_4  Feeling thermometer 0–100 (100=like → reversed)
#   cps21_groupdiscrim_1  Perceived discrimination 1=great deal → 5=none (as-is, higher=negative)
#   pes21_ab_favors       "No special favors" 1=SD → 5=SA (as-is, higher=negative)
#   pes21_ab_deserve      "Got less than they deserve" 1=SD → 5=SA (REVERSED, higher=negative)
#   pes21_ab_col          Colonial/structural attribution 1=SD → 5=SA (REVERSED, higher=negative)
#
# Scaling to [0,1]:
#   therm:      (100 - raw) / 100            raw range [0,100], DK=-99 → NA
#   1–5 items:  (corrected_value - 1) / 4    corrected range [1,5], DK=6 → NA
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

# devtools::install_github("clessn/sondr")
library(sondr)
library(dplyr)
library(tidyr)
library(haven)
library(here)

setwd(here::here())


# =============================================================================
# 1. LOAD DATA
# =============================================================================

ces <- readRDS("data/ces2021.rds")
cat("CES 2021 loaded:", nrow(ces), "respondents x", ncol(ces), "variables\n")


# =============================================================================
# 2. INSPECT ATTRIBUTES — verify coding before recoding
# =============================================================================

cat("\n=== ATTRIBUTE AUDIT ===\n")

vars_to_audit <- c("cps21_groups_therm_4", "cps21_groupdiscrim_1",
                   "pes21_ab_favors", "pes21_ab_deserve", "pes21_ab_col")

for (v in vars_to_audit) {
  cat("\n---", v, "---\n")
  attrs <- attributes(ces[[v]])
  cat("  label:  ", attrs$label, "\n")
  if (!is.null(attrs$labels)) {
    cat("  labels: ")
    print(attrs$labels)
  }
  cat("  range:  ", range(as.numeric(ces[[v]]), na.rm = TRUE), "\n")
}


# =============================================================================
# 3. RECODE — DIRECTION CORRECTION + RESCALE TO [0, 1]
# =============================================================================
# All items: higher = more negative perception of Indigenous peoples

ces <- ces %>%
  mutate(

    # --- Feeling thermometer ---
    # Raw: 0–100 (0=dislike, 100=like), -99 = DK
    # Step 1: DK → NA
    # Step 2: reverse (100 - raw) so higher = more negative
    # Step 3: divide by 100 to rescale to [0, 1]
    therm_neg = {
      raw <- as.numeric(cps21_groups_therm_4)
      raw[raw == -99] <- NA
      (100 - raw) / 100
    },

    # --- Perceived discrimination ---
    # Raw: 1=great deal, 2=a lot, 3=moderate, 4=a little, 5=none at all, 6=DK
    # Direction: higher already = less perceived discrimination = more negative
    # Step 1: DK (6) → NA
    # Step 2: rescale [1,5] → [0,1]: (x - 1) / 4
    groupdiscrim_neg = {
      x <- as.numeric(cps21_groupdiscrim_1)
      x[x == 6] <- NA
      (x - 1) / 4
    },

    # --- "No special favors" (racial resentment item) ---
    # Raw: 1=SD, 2=SomD, 3=Neither, 4=SomA, 5=SA, 6=DK
    # Direction: SA (5) = agree with bootstrap argument = more resentment = more negative → keep
    # Step 1: DK (6) → NA
    # Step 2: rescale [1,5] → [0,1]: (x - 1) / 4
    ab_favors_neg = {
      x <- as.numeric(pes21_ab_favors)
      x[x == 6] <- NA
      (x - 1) / 4
    },

    # --- "Got less than they deserve" (sympathy item) ---
    # Raw: 1=SD, 2=SomD, 3=Neither, 4=SomA, 5=SA, 6=DK
    # Direction: SA (5) = MORE sympathy = more POSITIVE → REVERSE before rescaling
    # Step 1: DK (6) → NA
    # Step 2: reverse: (6 - x) so SA(5) → 1, SD(1) → 5
    # Step 3: rescale [1,5] → [0,1]: (reversed - 1) / 4
    ab_deserve_neg = {
      x <- as.numeric(pes21_ab_deserve)
      x[x == 6] <- NA
      ((6 - x) - 1) / 4
    },

    # --- Structural attribution (colonialism/discrimination) ---
    # Raw: 1=SD, 2=SomD, 3=Neither, 4=SomA, 5=SA, 6=DK
    # Direction: SA (5) = endorses structural cause = more POSITIVE → REVERSE
    # Step 1: DK (6) → NA
    # Step 2: reverse: (6 - x) so SA(5) → 1, SD(1) → 5
    # Step 3: rescale [1,5] → [0,1]: (reversed - 1) / 4
    ab_col_neg = {
      x <- as.numeric(pes21_ab_col)
      x[x == 6] <- NA
      ((6 - x) - 1) / 4
    }

  )

# Sanity check: all items should be in [0, 1]
cat("\n=== RECODED ITEMS — RANGE CHECK (all should be [0,1]) ===\n")
scale_vars <- c("therm_neg", "groupdiscrim_neg", "ab_favors_neg", "ab_deserve_neg", "ab_col_neg")
for (v in scale_vars) {
  r <- range(ces[[v]], na.rm = TRUE)
  n_valid <- sum(!is.na(ces[[v]]))
  cat(sprintf("  %-20s range: [%.2f, %.2f]  N valid: %d\n", v, r[1], r[2], n_valid))
}


# =============================================================================
# 4. N COVERAGE CHECK
# =============================================================================

scale_items <- ces %>% select(all_of(scale_vars))

cat("\n=== N COVERAGE ===\n")
for (v in scale_vars) {
  cat(sprintf("  %-20s N valid: %d\n", v, sum(!is.na(ces[[v]]))))
}
cat(sprintf("  %-20s N valid (all 5): %d\n", "complete cases", sum(complete.cases(scale_items))))


# =============================================================================
# 5. FACTOR ANALYSIS — sondr::topdown_fa()
# =============================================================================
# topdown_fa() handles drop_na() internally.
# Passing all 5 items; FA will reveal whether this is 1 or 2 factors.

cat("\n=== FACTOR ANALYSIS (sondr::topdown_fa) ===\n")
fa_result <- sondr::topdown_fa(scale_items)
print(fa_result)


# =============================================================================
# 6. SAVE RECODED ITEMS
# =============================================================================

ces_scale <- ces %>%
  select(
    feduid, fedname,
    cps21_province,
    cps21_vismin_4,
    all_of(scale_vars)
  )

dir.create("data-clean", showWarnings = FALSE)
saveRDS(ces_scale, "data-clean/ces2021_scale_items.rds")
cat("\nSaved recoded scale items → data-clean/ces2021_scale_items.rds\n")
cat("N rows:", nrow(ces_scale), "\n")
