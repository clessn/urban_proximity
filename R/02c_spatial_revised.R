# =============================================================================
# 02c_spatial_revised.R
# Revised Step 2: Correct centroid computation
#   - Subtract reserve polygons from riding polygons
#   - Recompute riding centroids on non-reserve land only
#   - Recompute all distances
# =============================================================================

library(sf)
library(dplyr)
library(readr)

setwd("/home/ral/.openclaw/workspace/urban_proximity")
options(warn = 1)

cat("Loading shapefiles...\n")
fed <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet=TRUE) |>
  st_transform(3347)
csd <- st_read("data/shapefiles/csd_boundary/lcsd000b21a_e.shp", quiet=TRUE) |>
  st_transform(3347)
reserves <- csd |> filter(CSDTYPE == "IRI")
cat("  FEDs:", nrow(fed), " | Reserves:", nrow(reserves), "\n")

# --- Fix geometry validity ---
fed     <- st_make_valid(fed)
reserves <- st_make_valid(reserves)

# =============================================================================
# 1. For each riding: subtract all reserves inside it → "non-reserve riding area"
# =============================================================================
cat("\nSubtracting reserve polygons from ridings...\n")

# Union of all reserves (single geometry for efficient difference)
reserves_union <- st_union(reserves)
cat("  Reserve union computed.\n")

# Difference: riding - reserves inside it
fed_nonreserve <- st_difference(fed, reserves_union)
fed_nonreserve <- st_make_valid(fed_nonreserve)
cat("  Subtraction complete.\n")

# Check area reduction
fed$area_orig_km2     <- as.numeric(st_area(fed)) / 1e6
fed_nonreserve$area_nonres_km2 <- as.numeric(st_area(fed_nonreserve)) / 1e6
area_comparison <- data.frame(
  FEDUID = fed$FEDUID,
  FEDNAME = fed$FEDNAME,
  area_orig_km2 = fed$area_orig_km2,
  area_nonres_km2 = fed_nonreserve$area_nonres_km2
) |>
  mutate(
    area_removed_km2 = area_orig_km2 - area_nonres_km2,
    pct_removed = 100 * area_removed_km2 / area_orig_km2
  ) |>
  arrange(desc(pct_removed))

cat("\n  Top 10 ridings most affected by reserve subtraction:\n")
print(head(area_comparison[, c("FEDNAME","area_orig_km2","area_nonres_km2","pct_removed")], 10))

# =============================================================================
# 2. Recompute riding centroids on non-reserve area
# =============================================================================
cat("\nComputing corrected centroids (non-reserve area)...\n")

# Use centroid of the non-reserve polygon (most representative of non-Indigenous population)
fed_centroids_corrected <- fed_nonreserve |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  mutate(geometry = centroid) |>
  st_as_sf(crs = 3347) |>
  select(FEDUID, FEDNAME, PRUID, LANDAREA, area_nonres_km2 = area_nonres_km2, geometry)

# Original centroids for comparison
fed_centroids_orig <- fed |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  mutate(geometry = centroid) |>
  st_as_sf(crs = 3347)

# Distance between old and new centroid for each riding
centroid_shift <- as.numeric(st_distance(fed_centroids_orig, fed_centroids_corrected, by_element=TRUE)) / 1000
cat("\n  Centroid shift (km) after reserve subtraction:\n")
print(summary(centroid_shift))
cat("  Ridings with >10km shift:", sum(centroid_shift > 10), "\n")
cat("  Ridings with >50km shift:", sum(centroid_shift > 50), "\n")

# =============================================================================
# 3. Load CWB and prepare reserves
# =============================================================================
cwb <- read_csv("data/cwb/CWB_2021.csv", show_col_types=FALSE) |>
  rename(
    CSDUID_num = `CSD Code 2021`,
    cwb_score  = `CWB 2021`,
    pop_2021   = `Census Population 2021`,
    comm_type  = `Community Type 2021`
  ) |>
  mutate(CSDUID = sprintf("%07d", CSDUID_num))

reserves_cwb <- reserves |>
  left_join(cwb |> select(CSDUID, cwb_score, pop_2021, comm_type), by="CSDUID")

# Use a population threshold so proximity captures exposure to inhabited reserve
# communities rather than reserve parcels with little or no resident population.
reserves_inhabited <- reserves_cwb |>
  filter(!is.na(pop_2021), pop_2021 >= 100)

# CWB quartiles
cwb_breaks <- quantile(reserves_inhabited$cwb_score, probs=c(0,.25,.5,.75,1), na.rm=TRUE)
cat("\nCWB quartile breaks:", cwb_breaks, "\n")
reserves_inhabited <- reserves_inhabited |>
  mutate(
    cwb_quartile = cut(cwb_score, breaks=cwb_breaks,
                       labels=c("Q1_lowest","Q2","Q3","Q4_highest"),
                       include.lowest=TRUE)
  )

# Reserve centroids
reserves_centroids <- reserves_inhabited |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  mutate(geometry = centroid) |>
  st_as_sf(crs = 3347)

# CMA/CA urban classification for inhabited reserve centroids
cma <- st_read("data/shapefiles/cma_boundary/lcma000b21a_e.shp", quiet = TRUE) |>
  st_transform(3347)

reserves_centroids <- st_join(
  reserves_centroids,
  cma |> select(CMANAME, CMATYPE),
  left = TRUE
) |>
  mutate(
    urban_class = case_when(
      CMATYPE == "B" ~ "urban_cma",
      CMATYPE %in% c("D", "H") ~ "semi_urban_ca",
      TRUE ~ "rural_remote"
    )
  )

# =============================================================================
# 4. Recompute distance matrix
# =============================================================================
cat("\nComputing corrected distance matrix (corrected riding centroids)...\n")
dist_matrix <- st_distance(fed_centroids_corrected, reserves_centroids)
cat("  Distance matrix:", dim(dist_matrix), "\n")

fed_prox <- fed_centroids_corrected |>
  st_drop_geometry() |>
  select(FEDUID, FEDNAME, PRUID, area_nonres_km2) |>
  mutate(FEDUID = as.numeric(FEDUID)) |>
  mutate(
    dist_nearest_reserve_km  = as.numeric(apply(dist_matrix, 1, min)) / 1000,
    nearest_reserve_idx      = apply(dist_matrix, 1, which.min),
    nearest_reserve_uid      = reserves_centroids$CSDUID[nearest_reserve_idx],
    nearest_reserve_name     = reserves_centroids$CSDNAME[nearest_reserve_idx],
    nearest_reserve_cwb      = reserves_centroids$cwb_score[nearest_reserve_idx],
    nearest_reserve_urban    = reserves_centroids$comm_type[nearest_reserve_idx],
    nearest_reserve_urban_class = reserves_centroids$urban_class[nearest_reserve_idx],
    nearest_reserve_pop      = reserves_centroids$pop_2021[nearest_reserve_idx],
    log_dist_nearest_km      = log1p(dist_nearest_reserve_km),
    log_area_nonres_km2      = log1p(area_nonres_km2)
  )

fed_prox$urban_class <- fed_prox$nearest_reserve_urban_class

# By CWB quartile
for (q in c("Q1_lowest","Q2","Q3","Q4_highest")) {
  res_q <- reserves_centroids |> filter(cwb_quartile == q)
  if (nrow(res_q) == 0) { fed_prox[[paste0("dist_q_",q,"_km")]] <- NA; next }
  d <- st_distance(fed_centroids_corrected, res_q)
  fed_prox[[paste0("dist_q_",q,"_km")]] <- as.numeric(apply(d, 1, min)) / 1000
}

# Also add riding area original + centroid shift
fed_prox$area_orig_km2  <- fed$area_orig_km2[match(as.character(fed_prox$FEDUID), fed$FEDUID)]
fed_prox$centroid_shift_km <- centroid_shift

cat("\nCorrected distance to nearest reserve:\n")
print(summary(fed_prox$dist_nearest_reserve_km))

# =============================================================================
# 5. Save
# =============================================================================
saveRDS(fed_prox, "data-clean/proximity_fed_corrected.rds")
write_csv(fed_prox, "data-clean/proximity_fed_corrected.csv")

# Side-by-side comparison with original
prox_orig <- read_csv("data-clean/proximity_fed_enhanced.csv", show_col_types=FALSE)
prox_orig_sub <- prox_orig |> select(FEDUID, dist_orig=dist_nearest_reserve_km, cwb_orig=nearest_reserve_cwb)
comparison <- fed_prox |> select(FEDUID, dist_corrected=dist_nearest_reserve_km, cwb_corrected=nearest_reserve_cwb) |>
  left_join(prox_orig_sub, by="FEDUID") |>
  mutate(dist_diff = dist_corrected - dist_orig)

cat("\n=== IMPACT OF CORRECTION ===\n")
cat("Change in distance to nearest reserve (corrected - original):\n")
print(summary(comparison$dist_diff))
cat("Ridings where nearest reserve changed:", sum(comparison$cwb_corrected != comparison$cwb_orig, na.rm=TRUE), "\n")

cat("\n=== DONE ===\n")
