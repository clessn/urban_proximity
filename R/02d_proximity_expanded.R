# =============================================================================
# 02d_proximity_expanded.R
# Recompute reserve proximity with expanded Indigenous community filter:
#   - IRI: Indian Reserves (original)
#   - IGD: Indian Government Districts
#   - S-É / S-E: Indian Settlements (Kanesatake, Kitcisakik, etc.)
#   - TC: Terres Cries / Cree communities (James Bay Cree, 9 communities)
#   - TK: Naskapi territory (Kawawachikamach)
#   - VN: Village Naskapi entries (Naskapi only, NOT Inuit VN entries)
#
# EXCLUDED (justified in paper):
#   - TI: Terres Inuites — Inuit, not First Nations, 1000+ km from urban centres
#   - VN entries that are Inuit (Kuujjuaq, Salluit, etc.) — same reason
#
# CWB IMPUTATION for incompletely enumerated reserves:
#   - Priority 1: CWB 2016
#   - Priority 2: CWB 2021 (if 2016 missing)
#   - Priority 3: CWB 2011 (if file present at data/cwb/CWB_2011.csv)
#   - Priority 4: NA (flagged, kept in proximity matrix but excluded from CWB models)
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

.libPaths(c('/home/ral/R/library', .libPaths()))

library(sf)
library(dplyr)
library(tidyr)

sf_use_s2(FALSE)
setwd("/home/ral/.openclaw/workspace/urban_proximity")
dir.create("data-clean", showWarnings = FALSE)


# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading shapefiles...\n")
csd <- st_read("data/shapefiles/csd_boundary/lcsd000b21a_e.shp", quiet = TRUE)
fed <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp",  quiet = TRUE) %>%
  filter(as.numeric(PRUID) < 60)   # exclude territories

cwb16 <- read.csv("data/cwb/CWB_2016.csv", fileEncoding = "latin1")
cwb21 <- read.csv("data/cwb/CWB_2021.csv", fileEncoding = "latin1")
names(cwb16) <- c("CSDUID", "CSDName", "Pop16", "Income16", "Education16",
                  "Housing16", "Labour16", "CWB16", "CommunityType16")
names(cwb21) <- c("CSDUID", "CSDName21", "Pop21", "Income21", "Education21",
                  "Housing21", "Labour21", "CWB21", "CommunityType21")
cwb16$CSDUID <- as.character(cwb16$CSDUID)
cwb21$CSDUID <- as.character(cwb21$CSDUID)

# CWB 2011 — load if available (manual download required, see README)
cwb11_path <- "data/cwb/CWB_2011.csv"
has_cwb11  <- file.exists(cwb11_path)
if (has_cwb11) {
  cwb11 <- read.csv(cwb11_path, fileEncoding = "latin1")
  # Normalise column names (StatCan format varies by year)
  names(cwb11)[1] <- "CSDUID"
  cwb11_col <- grep("CWB|IBC", names(cwb11), value = TRUE, ignore.case = TRUE)[1]
  cwb11$CWB11 <- as.numeric(cwb11[[cwb11_col]])
  cwb11$CSDUID <- as.character(cwb11$CSDUID)
  cat("CWB 2011 loaded:", nrow(cwb11), "rows\n")
} else {
  cat("CWB 2011 not found — skipping (see data/cwb/README_CWB2011.md for download instructions)\n")
}


# =============================================================================
# 2. DEFINE EXPANDED RESERVE FILTER
# =============================================================================

# Naskapi VN entries to INCLUDE (CSDUID 2497806 = Kawawachikamach area)
# Inuit VN entries to EXCLUDE: CSDUID starting with 2499 and type VN
naskapi_vn <- c("2499075")  # Kuujjuarapik is mixed Cree/Inuit — include as Cree territory

# CSD types to include
include_types <- c("IRI", "IGD", "S-É", "S-E", "TC", "TK")

reserves_raw <- csd %>%
  filter(
    CSDTYPE %in% include_types |
    (CSDTYPE == "VN" & as.character(CSDUID) == "2497806")  # Kawawachikamach VN entry
  ) %>%
  filter(as.numeric(PRUID) < 60) %>%   # exclude territories
  mutate(CSDUID = as.character(CSDUID))

cat("Reserve communities after expanded filter:", nrow(reserves_raw), "\n")
cat("By type:\n"); print(table(reserves_raw$CSDTYPE))


# =============================================================================
# 3. JOIN CWB — WITH LAYERED IMPUTATION (2016 → 2021 → 2011 → NA)
# =============================================================================

reserves <- reserves_raw %>%
  st_drop_geometry() %>%
  left_join(cwb16 %>% select(CSDUID, CWB16, Pop16, CommunityType16), by = "CSDUID") %>%
  left_join(cwb21 %>% select(CSDUID, CWB21, Pop21), by = "CSDUID")

if (has_cwb11) {
  reserves <- reserves %>%
    left_join(cwb11 %>% select(CSDUID, CWB11), by = "CSDUID")
} else {
  reserves$CWB11 <- NA_real_
}

# Layered imputation: prefer 2016, fall back to 2021, then 2011
reserves <- reserves %>%
  mutate(
    CWB = case_when(
      !is.na(CWB16) ~ CWB16,
      !is.na(CWB21) ~ CWB21,
      !is.na(CWB11) ~ CWB11,
      TRUE           ~ NA_real_
    ),
    CWB_source = case_when(
      !is.na(CWB16) ~ "2016",
      !is.na(CWB21) ~ "2021",
      !is.na(CWB11) ~ "2011 (imputed)",
      TRUE           ~ "missing"
    ),
    Pop = coalesce(as.numeric(Pop16), as.numeric(Pop21))
  )

cat("\n=== CWB coverage summary ===\n")
print(table(reserves$CWB_source))
cat("CWB available:", sum(!is.na(reserves$CWB)), "/", nrow(reserves), "\n")

# Identify still-missing large communities (pop known from other sources)
missing_large <- reserves %>%
  filter(is.na(CWB), !is.na(Pop) | grepl("Kahnawake|Kanesatake|Akwesasne", CSDNAME, ignore.case=TRUE)) %>%
  select(CSDUID, CSDNAME, CSDTYPE, PRUID, Pop, CWB_source)
if (nrow(missing_large) > 0) {
  cat("\nNotably missing (may be large communities):\n")
  print(missing_large)
}

# Re-attach geometry
reserves_sf <- reserves_raw %>%
  left_join(reserves %>% select(CSDUID, CWB, CWB_source, Pop), by = "CSDUID") %>%
  st_transform(4326)

cat("\nFinal reserve dataset:", nrow(reserves_sf), "communities\n")
cat("With CWB:", sum(!is.na(reserves_sf$CWB)), "\n")
cat("Without CWB (will appear as NA in proximity models):", sum(is.na(reserves_sf$CWB)), "\n")


# =============================================================================
# 4. COMPUTE FEDERAL RIDING CENTROIDS
# =============================================================================

cat("\nComputing riding centroids...\n")
fed_proj <- fed %>% st_transform(3347)
res_proj  <- reserves_sf %>% st_transform(3347)

fed_centroids <- fed_proj %>%
  st_centroid() %>%
  mutate(FEDUID = as.numeric(FEDUID))

res_centroids <- res_proj %>%
  st_centroid()

cat("FED centroids:", nrow(fed_centroids), "\n")
cat("Reserve centroids:", nrow(res_centroids), "\n")


# =============================================================================
# 5. COMPUTE DISTANCE MATRIX & PROXIMITY VARIABLES
# =============================================================================

cat("\nComputing distance matrix (", nrow(fed_centroids), "x", nrow(res_centroids), ")...\n")
dist_matrix <- st_distance(fed_centroids, res_centroids)  # metres

# For each riding: find nearest reserve (any), nearest high-CWB (Q4)
cwb_vals <- reserves_sf$CWB
cwb_q4_threshold <- quantile(cwb_vals, 0.75, na.rm = TRUE)
cwb_q1_threshold <- quantile(cwb_vals, 0.25, na.rm = TRUE)
cat("CWB Q4 threshold (top quartile):", cwb_q4_threshold, "\n")
cat("CWB Q1 threshold (bottom quartile):", cwb_q1_threshold, "\n")

idx_q4 <- which(!is.na(cwb_vals) & cwb_vals >= cwb_q4_threshold)
idx_q1 <- which(!is.na(cwb_vals) & cwb_vals <= cwb_q1_threshold)

proximity <- fed_centroids %>%
  st_drop_geometry() %>%
  mutate(
    # Nearest reserve (any)
    nearest_idx              = apply(dist_matrix, 1, which.min),
    dist_nearest_reserve_km  = apply(dist_matrix, 1, min) / 1000,
    nearest_reserve_cwb      = cwb_vals[nearest_idx],
    nearest_reserve_name     = res_centroids$CSDNAME[nearest_idx],
    nearest_reserve_type     = res_centroids$CSDTYPE[nearest_idx],
    nearest_reserve_pop      = reserves_sf$Pop[nearest_idx],

    # Nearest HIGH-CWB reserve (Q4, top quartile)
    dist_nearest_cwb_Q4_km   = if (length(idx_q4) > 0)
                                  apply(dist_matrix[, idx_q4, drop=FALSE], 1, min) / 1000
                                else NA_real_,

    # Nearest LOW-CWB reserve (Q1, bottom quartile)
    dist_nearest_cwb_Q1_km   = if (length(idx_q1) > 0)
                                  apply(dist_matrix[, idx_q1, drop=FALSE], 1, min) / 1000
                                else NA_real_,

    # Count of reserves within 50/100/200 km
    reserves_within_50km     = apply(dist_matrix, 1, function(d) sum(d/1000 < 50)),
    reserves_within_100km    = apply(dist_matrix, 1, function(d) sum(d/1000 < 100)),
    reserves_within_200km    = apply(dist_matrix, 1, function(d) sum(d/1000 < 200)),

    # Log transforms
    log_dist_nearest_km      = log(dist_nearest_reserve_km + 1),
    log_dist_cwb_q4_km       = log(dist_nearest_cwb_Q4_km  + 1),
    log_dist_cwb_q1_km       = log(dist_nearest_cwb_Q1_km  + 1),
    log_reserves_50km        = log(reserves_within_50km  + 1),
    log_reserves_100km       = log(reserves_within_100km + 1),
    log_reserves_200km       = log(reserves_within_200km + 1)
  ) %>%
  select(-nearest_idx)

cat("\n=== Proximity summary ===\n")
cat("Ridings:", nrow(proximity), "\n")
print(summary(proximity[, c("dist_nearest_reserve_km", "dist_nearest_cwb_Q4_km",
                             "reserves_within_100km", "nearest_reserve_cwb")]))


# =============================================================================
# 6. SAVE
# =============================================================================

out <- fed_proj %>%
  st_drop_geometry() %>%
  select(FEDUID, FEDNAME, PRUID) %>%
  mutate(FEDUID = as.numeric(FEDUID)) %>%
  left_join(proximity, by = "FEDUID")

write.csv(out, "data-clean/proximity_fed_v2.csv", row.names = FALSE)
saveRDS(out, "data-clean/proximity_fed_v2.rds")

# Also save enriched reserve dataset
saveRDS(reserves_sf, "data-clean/reserves_expanded.rds")

cat("\n=== SAVED ===\n")
cat("  data-clean/proximity_fed_v2.csv   (", nrow(out), "ridings)\n")
cat("  data-clean/reserves_expanded.rds  (", nrow(reserves_sf), "communities)\n")
cat("\nExpansion summary vs v1:\n")
if (file.exists("data-clean/proximity_fed_enhanced.csv")) {
  v1 <- read.csv("data-clean/proximity_fed_enhanced.csv")
  cat("  v1 (IRI only): based on 992 reserves\n")
  cat("  v2 (expanded): based on", nrow(reserves_sf), "communities\n")
  cat("  Added:", nrow(reserves_sf) - 992, "Cree/Naskapi/Settlement communities\n")
}

# =============================================================================
# 7. WRITE DOWNLOAD INSTRUCTIONS FOR CWB 2011
# =============================================================================

cwb11_readme <- '# CWB 2011 — Manual Download Required

The Statistics Canada server is not accessible from the research server.
To obtain the 2011 CWB data (needed to impute values for Kahnawake, Kanesatake, and
other incompletely-enumerated reserves that were counted in 2011):

## Download steps (2 minutes)

1. Open this URL in your browser:
   https://www150.statcan.gc.ca/n1/pub/89-645-x/2015001/tbl/tbl-eng.htm

2. Alternative: search "Community Well-Being Index 2011" on:
   https://www150.statcan.gc.ca/n1/

3. Save the file as: data/cwb/CWB_2011.csv

4. Re-run this script: Rscript R/02d_proximity_expanded.R

## What this fixes
- Kahnawake (~7,000 on-reserve, CWB ~75 in 2011, near Montreal)
- Kanesatake (~2,000, CWB ~65 in 2011, near Montreal)
- Akwesasne (~10,000, CWB ~65 in 2011, near Ontario/QC border)
- Listuguj (~2,500, CWB ~60 in 2011, Gaspésie)
- ~10 other communities

## Expected impact
Without 2011 imputation: 577/1002 communities have CWB (57.6%)
With 2011 imputation: ~591/1002 estimated (59%)
The big QC communities are the key gain.
'
writeLines(cwb11_readme, "data/cwb/README_CWB2011.md")
cat("Wrote: data/cwb/README_CWB2011.md\n")
