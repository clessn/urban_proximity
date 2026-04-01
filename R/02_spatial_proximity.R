# =============================================================================
# 02_spatial_proximity.R
# Step 2: Build spatial proximity measure
#   - Extract Indian Reserves (IRI) from CSD shapefile
#   - Compute reserve centroids
#   - Join CWB index (well-being score) to each reserve
#   - Load FED boundary file, compute riding centroids
#   - For each riding: compute distance to nearest reserve
#     + distance to nearest reserve by CWB quartile
#   - Classify reserves: urban (within CMA/CA) vs. rural/remote
#   - Save proximity dataset → data-clean/proximity_fed.rds
#
# Project: Urban Proximity & Indigenous Attitudes
# =============================================================================

library(sf)
library(dplyr)
library(readr)
library(units)

setwd("/home/ral/.openclaw/workspace/urban_proximity")

# =============================================================================
# 1. Load and filter Indian Reserves (IRI) from CSD shapefile
# =============================================================================
cat("Loading CSD shapefile...\n")
csd <- st_read("data/shapefiles/csd_boundary/lcsd000b21a_e.shp", quiet = TRUE)
cat("  Total CSDs:", nrow(csd), "\n")

# Filter to Indian Reserves and Crown Lands (type IRI)
# IRI = Indian Reserve (type code used by Statistics Canada)
reserves <- csd %>%
  filter(CSDTYPE == "IRI") %>%
  select(CSDUID, CSDNAME, PRUID, LANDAREA, geometry)

cat("  Indian Reserves (IRI):", nrow(reserves), "\n")
print(head(reserves[, c("CSDUID", "CSDNAME", "PRUID")], 5))


# =============================================================================
# 2. Join CWB 2021 index to reserves
# =============================================================================
cat("\nJoining CWB 2021...\n")
cwb <- read_csv("data/cwb/CWB_2021.csv", show_col_types = FALSE) %>%
  rename(
    CSDUID_num   = `CSD Code 2021`,
    csd_name_cwb = `CSD Name 2021`,
    pop_2021     = `Census Population 2021`,
    cwb_income   = `Income 2021`,
    cwb_educ     = `Education 2021`,
    cwb_housing  = `Housing 2021`,
    cwb_labour   = `Labour Force Activity 2021`,
    cwb_score    = `CWB 2021`,
    comm_type    = `Community Type 2021`
  ) %>%
  mutate(CSDUID = sprintf("%07d", CSDUID_num))

# Join
reserves <- reserves %>%
  left_join(
    cwb %>% select(CSDUID, pop_2021, cwb_score, cwb_income, cwb_educ,
                   cwb_housing, cwb_labour, comm_type),
    by = "CSDUID"
  )

cat("  Reserves with CWB score:", sum(!is.na(reserves$cwb_score)), "/", nrow(reserves), "\n")
cat("  CWB score distribution for reserves:\n")
print(summary(reserves$cwb_score))



# =============================================================================
# 3. Classify reserves: urban vs. rural/remote
# =============================================================================
# Strategy: download Statistics Canada population centre boundary file
# and use spatial join to classify reserves as:
#   - Urban: within a CMA (Census Metropolitan Area, pop >100K)
#   - Semi-urban: within a CA (Census Agglomeration, pop 10K-100K)
#   - Rural/remote: outside all CMAs/CAs
#
# Note: We use the CSD's own classification from the CWB "comm_type" field
# plus a derived urban flag based on whether the reserve is in a CMA/CA

# Download Population Centres boundary (CMA/CA) from StatsCan
popctr_zip <- "data/shapefiles/cma_boundary.zip"
if (!file.exists(popctr_zip)) {
  cat("  Downloading CMA/CA boundary file...\n")
  url <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcma000b21a_e.zip"
  download.file(url, popctr_zip, quiet = TRUE, method = "curl")
}
file_type <- system(paste("file", popctr_zip, "| cut -d: -f2"), intern = TRUE)
cat("  CMA zip type:", file_type, "\n")

if (grepl("Zip", file_type)) {
  unzip(popctr_zip, exdir = "data/shapefiles/cma_boundary/", overwrite = TRUE)
  cma_files <- list.files("data/shapefiles/cma_boundary/", pattern = "\\.shp$")
  cma <- st_read(paste0("data/shapefiles/cma_boundary/", cma_files[1]), quiet = TRUE)
  cat("  CMAs loaded:", nrow(cma), "\n")
  cat("  CMA types:\n")
  if ("CMATYPE" %in% names(cma)) print(table(cma$CMATYPE))
  
  # Reproject reserves to same CRS as CMA
  cma <- st_transform(cma, st_crs(reserves))
  
  # Spatial join: for each reserve centroid, find if inside a CMA or CA
  reserves_pt <- reserves %>% st_centroid()
  join_cma <- st_join(reserves_pt, cma %>% select(CMANAME, CMATYPE), left = TRUE)
  
  # Classify
  # CMATYPE: "B" = CMA, "D" = CA, "H" = small CA, NA = rural/remote
  reserves <- reserves %>%
    mutate(
      urban_class = case_when(
        join_cma$CMATYPE %in% c("B") ~ "urban_cma",      # CMA (>100K)
        join_cma$CMATYPE %in% c("D", "H") ~ "semi_urban_ca",  # CA (10K-100K)
        TRUE ~ "rural_remote"
      ),
      cma_name = join_cma$CMANAME
    )
} else {
  cat("  WARNING: CMA file download failed — using province as proxy for urban\n")
  reserves <- reserves %>%
    mutate(urban_class = "unknown", cma_name = NA)
}

cat("\n  Urban classification of reserves:\n")
print(table(reserves$urban_class))


# =============================================================================
# 3b. Restrict exposure measures to inhabited reserves
# =============================================================================
# Use a population threshold so proximity captures exposure to actual communities
# rather than uninhabited land parcels that still carry reserve boundaries.
reserves_inhabited <- reserves %>%
  filter(!is.na(pop_2021), pop_2021 >= 100)

cat("  Inhabited reserves (pop_2021 >= 100):", nrow(reserves_inhabited), "\n")


# =============================================================================
# 4. Compute reserve centroids in projected CRS (NAD83 / Canada Albers Equal Area)
# =============================================================================
cat("\nComputing reserve centroids...\n")
# Project to Canada Albers Equal Area (EPSG:3347) for accurate distance measurement
reserves_albers <- reserves %>%
  st_transform(3347) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  mutate(geometry = centroid) %>%
  st_as_sf(crs = 3347)

reserves_inhabited_albers <- reserves_inhabited %>%
  st_transform(3347) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  mutate(geometry = centroid) %>%
  st_as_sf(crs = 3347)

cat("  Reserve centroids computed:", nrow(reserves_albers), "\n")
cat("  Inhabited reserve centroids computed:", nrow(reserves_inhabited_albers), "\n")


# =============================================================================
# 5. Load FED (Federal Electoral District) shapefile, compute centroids
# =============================================================================
cat("\nLoading FED shapefile...\n")
fed <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet = TRUE)
cat("  FEDs loaded:", nrow(fed), "\n")
cat("  Columns:", paste(names(fed), collapse = ", "), "\n")

fed_albers <- fed %>%
  st_transform(3347) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  mutate(geometry = centroid) %>%
  st_as_sf(crs = 3347)


# =============================================================================
# 6. Distance matrix: each riding → nearest reserve(s) by CWB tier
# =============================================================================
cat("\nComputing proximity measures...\n")

# Use inhabited reserves for exposure/nearest-reserve measures so distance reflects
# nearby communities rather than uninhabited reserve parcels.
dist_matrix <- st_distance(fed_albers, reserves_inhabited_albers)  # n_fed x n_reserve matrix
cat("  Distance matrix dimensions:", dim(dist_matrix), "\n")

# For each FED: distance to nearest reserve (any)
fed_prox <- fed_albers %>%
  st_drop_geometry() %>%
  select(FEDUID, FEDNAME, PRUID) %>%
  mutate(
    dist_nearest_reserve_km = as.numeric(
      apply(dist_matrix, 1, min) / 1000  # metres → km
    ),
    nearest_reserve_idx = apply(dist_matrix, 1, which.min),
    nearest_reserve_uid = reserves_inhabited_albers$CSDUID[nearest_reserve_idx],
    nearest_reserve_name = reserves_inhabited_albers$CSDNAME[nearest_reserve_idx],
    nearest_reserve_cwb = reserves_inhabited_albers$cwb_score[nearest_reserve_idx],
    nearest_reserve_urban = reserves_inhabited_albers$urban_class[nearest_reserve_idx],
    nearest_reserve_pop = reserves_inhabited_albers$pop_2021[nearest_reserve_idx]
  )

cat("  Distance to nearest reserve (km):\n")
print(summary(fed_prox$dist_nearest_reserve_km))


# =============================================================================
# 7. Distance to nearest reserve by CWB quartile
# =============================================================================
cat("\nComputing distances by CWB quartile...\n")

# CWB quartiles (among inhabited reserves with scores)
cwb_breaks <- quantile(reserves_inhabited_albers$cwb_score, probs = c(0, 0.25, 0.5, 0.75, 1),
                       na.rm = TRUE)
cat("  CWB quartile breaks:", cwb_breaks, "\n")

reserves_inhabited_albers <- reserves_inhabited_albers %>%
  mutate(
    cwb_quartile = cut(cwb_score,
                       breaks = cwb_breaks,
                       labels = c("Q1_lowest", "Q2", "Q3", "Q4_highest"),
                       include.lowest = TRUE)
  )

# Distance to nearest reserve in each CWB quartile
for (q in c("Q1_lowest", "Q2", "Q3", "Q4_highest")) {
  res_q <- reserves_inhabited_albers %>% filter(cwb_quartile == q)
  if (nrow(res_q) == 0) {
    fed_prox[[paste0("dist_nearest_cwb_", q, "_km")]] <- NA
    next
  }
  dist_q <- st_distance(fed_albers, res_q)
  fed_prox[[paste0("dist_nearest_cwb_", q, "_km")]] <- as.numeric(
    apply(dist_q, 1, min) / 1000
  )
}

# Distance to nearest urban/semi-urban reserve
for (uclass in c("urban_cma", "semi_urban_ca", "rural_remote")) {
  res_u <- reserves_inhabited_albers %>% filter(urban_class == uclass)
  if (nrow(res_u) == 0) {
    fed_prox[[paste0("dist_nearest_", uclass, "_km")]] <- NA
    next
  }
  dist_u <- st_distance(fed_albers, res_u)
  fed_prox[[paste0("dist_nearest_", uclass, "_km")]] <- as.numeric(
    apply(dist_u, 1, min) / 1000
  )
}

cat("\n  Sample proximity data (first 6 rows):\n")
print(head(fed_prox[, c("FEDUID", "FEDNAME", "dist_nearest_reserve_km",
                         "nearest_reserve_cwb", "nearest_reserve_urban",
                         "dist_nearest_cwb_Q1_lowest_km",
                         "dist_nearest_cwb_Q4_highest_km")]))


# =============================================================================
# 8. Log-transform distances (heavy right skew expected)
# =============================================================================
fed_prox <- fed_prox %>%
  mutate(
    log_dist_nearest_km  = log1p(dist_nearest_reserve_km),
    log_dist_urban_km    = log1p(dist_nearest_urban_cma_km),
    log_dist_rural_km    = log1p(dist_nearest_rural_remote_km),
    log_dist_cwb_q1_km   = log1p(dist_nearest_cwb_Q1_lowest_km),
    log_dist_cwb_q4_km   = log1p(dist_nearest_cwb_Q4_highest_km)
  )


# =============================================================================
# 9. Save results
# =============================================================================
dir.create("data-clean", showWarnings = FALSE)
saveRDS(fed_prox, "data-clean/proximity_fed.rds")
write_csv(fed_prox, "data-clean/proximity_fed.csv")

# Also save reserves with CWB for inspection
saveRDS(reserves_albers, "data-clean/reserves_cwb.rds")

cat("\n=== STEP 2 COMPLETE ===\n")
cat("Proximity dataset saved:", nrow(fed_prox), "federal ridings\n")
cat("Variables:\n")
print(names(fed_prox))
cat("\nDistance summary:\n")
print(summary(fed_prox[, grep("dist_nearest|log_dist", names(fed_prox))]))
