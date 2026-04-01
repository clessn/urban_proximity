# =============================================================================
# 02b_spatial_proximity_buffers.R
# Step 2b: Add buffer counts and area calculations to proximity analysis
#   - For each FED, count reserves within 50km, 100km, 200km buffers
#   - Calculate total reserve land area within each buffer
#   - Generate summary statistics of geographic coverage
#   - Save enhanced proximity dataset
#
# Project: Urban Proximity & Indigenous Attitudes
# =============================================================================

library(sf)
library(dplyr)
library(readr)
library(units)
library(purrr)

setwd("/home/ral/.openclaw/workspace/urban_proximity")

# =============================================================================
# 1. Load existing data
# =============================================================================
cat("Loading existing proximity data...\n")
fed_prox <- readRDS("data-clean/proximity_fed.rds")
reserves_albers <- readRDS("data-clean/reserves_cwb.rds")
fed_albers <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet = TRUE) %>%
  st_transform(3347)

cat("  FEDs:", nrow(fed_albers), "\n")
cat("  Reserves:", nrow(reserves_albers), "\n")

# =============================================================================
# 2. Create buffers around FED centroids
# =============================================================================
cat("\nCreating FED centroids for buffer analysis...\n")
fed_centroids <- fed_albers %>%
  st_centroid()

# Define buffer distances (in meters)
buffer_distances <- c(50, 100, 200) * 1000  # Convert km to meters

# =============================================================================
# 3. Count reserves within each buffer
# =============================================================================
cat("\nCounting reserves within buffers...\n")

# Function to count reserves within buffer
count_reserves_in_buffer <- function(centroid, buffer_m, reserves) {
  buffer <- st_buffer(centroid, buffer_m)
  # Count reserves whose centroids fall within buffer
  within <- st_intersects(buffer, reserves, sparse = FALSE)
  sum(within)
}

# Function to calculate total area within buffer
sum_area_in_buffer <- function(centroid, buffer_m, reserves) {
  buffer <- st_buffer(centroid, buffer_m)
  # Find reserves whose centroids fall within buffer
  within <- st_intersects(buffer, reserves, sparse = FALSE)
  if (any(within)) {
    # Sum land area of reserves within buffer
    sum(reserves$LANDAREA[within], na.rm = TRUE)
  } else {
    0
  }
}

# Initialize columns
for (dist_km in c(50, 100, 200)) {
  fed_prox[[paste0("reserves_within_", dist_km, "km")]] <- 0
  fed_prox[[paste0("area_within_", dist_km, "km_sqkm")]] <- 0
}

# Process each FED (this may take a while)
cat("Processing", nrow(fed_centroids), "FEDs...\n")
pb <- txtProgressBar(min = 0, max = nrow(fed_centroids), style = 3)

for (i in 1:nrow(fed_centroids)) {
  centroid <- fed_centroids[i, ]
  
  for (dist_km in c(50, 100, 200)) {
    buffer_m <- dist_km * 1000
    
    # Count reserves
    count <- count_reserves_in_buffer(centroid, buffer_m, reserves_albers)
    fed_prox[i, paste0("reserves_within_", dist_km, "km")] <- count
    
    # Calculate area (if any reserves)
    if (count > 0) {
      area <- sum_area_in_buffer(centroid, buffer_m, reserves_albers)
      # LANDAREA is already in square kilometers
      fed_prox[i, paste0("area_within_", dist_km, "km_sqkm")] <- area
    }
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

# =============================================================================
# 4. Generate summary statistics
# =============================================================================
cat("\n\n=== GEOGRAPHIC COVERAGE SUMMARY ===\n")

for (dist_km in c(50, 100, 200)) {
  count_col <- paste0("reserves_within_", dist_km, "km")
  area_col <- paste0("area_within_", dist_km, "km_sqkm")
  
  n_with_reserves <- sum(fed_prox[[count_col]] > 0)
  pct_with_reserves <- round(n_with_reserves / nrow(fed_prox) * 100, 1)
  
  total_reserves_count <- sum(fed_prox[[count_col]])
  total_area <- sum(fed_prox[[area_col]], na.rm = TRUE)
  
  cat(sprintf("\nWithin %d km buffer:\n", dist_km))
  cat(sprintf("  Ridings with ≥1 reserve: %d/%d (%0.1f%%)\n", 
              n_with_reserves, nrow(fed_prox), pct_with_reserves))
  cat(sprintf("  Total reserves counted: %d\n", total_reserves_count))
  cat(sprintf("  Total reserve area: %0.1f sq km\n", total_area))
  
  # Distribution of reserve counts
  cat("  Distribution of reserve counts per riding:\n")
  counts_table <- table(fed_prox[[count_col]])
  for (n in sort(unique(fed_prox[[count_col]]))) {
    if (n <= 10) {  # Show up to 10, then summarize
      cat(sprintf("    %2d reserves: %3d ridings\n", n, sum(fed_prox[[count_col]] == n)))
    }
  }
  if (max(fed_prox[[count_col]]) > 10) {
    n_gt_10 <- sum(fed_prox[[count_col]] > 10)
    cat(sprintf("    >10 reserves: %3d ridings\n", n_gt_10))
  }
}

# =============================================================================
# 5. Add derived variables
# =============================================================================
cat("\nAdding derived variables...\n")

# Binary indicators for presence of reserves
fed_prox <- fed_prox %>%
  mutate(
    has_reserve_50km = reserves_within_50km > 0,
    has_reserve_100km = reserves_within_100km > 0,
    has_reserve_200km = reserves_within_200km > 0,
    
    # Log-transform counts (add 1 to handle zeros)
    log_reserves_50km = log1p(reserves_within_50km),
    log_reserves_100km = log1p(reserves_within_100km),
    log_reserves_200km = log1p(reserves_within_200km),
    
    # Log-transform areas (add small constant to handle zeros)
    log_area_50km = log1p(area_within_50km_sqkm),
    log_area_100km = log1p(area_within_100km_sqkm),
    log_area_200km = log1p(area_within_200km_sqkm)
  )

# =============================================================================
# 6. Save enhanced dataset
# =============================================================================
cat("\nSaving enhanced proximity dataset...\n")

# Save as RDS and CSV
saveRDS(fed_prox, "data-clean/proximity_fed_enhanced.rds")
write_csv(fed_prox, "data-clean/proximity_fed_enhanced.csv")

# Also create a summary table
summary_stats <- data.frame(
  buffer_km = c(50, 100, 200),
  ridings_with_reserves = c(
    sum(fed_prox$has_reserve_50km),
    sum(fed_prox$has_reserve_100km),
    sum(fed_prox$has_reserve_200km)
  ),
  pct_with_reserves = c(
    round(mean(fed_prox$has_reserve_50km) * 100, 1),
    round(mean(fed_prox$has_reserve_100km) * 100, 1),
    round(mean(fed_prox$has_reserve_200km) * 100, 1)
  ),
  total_reserves_count = c(
    sum(fed_prox$reserves_within_50km),
    sum(fed_prox$reserves_within_100km),
    sum(fed_prox$reserves_within_200km)
  ),
  total_area_sqkm = c(
    sum(fed_prox$area_within_50km_sqkm, na.rm = TRUE),
    sum(fed_prox$area_within_100km_sqkm, na.rm = TRUE),
    sum(fed_prox$area_within_200km_sqkm, na.rm = TRUE)
  ),
  mean_reserves_per_riding = c(
    round(mean(fed_prox$reserves_within_50km), 2),
    round(mean(fed_prox$reserves_within_100km), 2),
    round(mean(fed_prox$reserves_within_200km), 2)
  ),
  median_reserves_per_riding = c(
    median(fed_prox$reserves_within_50km),
    median(fed_prox$reserves_within_100km),
    median(fed_prox$reserves_within_200km)
  )
)

write_csv(summary_stats, "data-clean/buffer_summary_stats.csv")

cat("\n=== STEP 2B COMPLETE ===\n")
cat("Enhanced proximity dataset saved with buffer counts and areas\n")
cat("Files created:\n")
cat("  data-clean/proximity_fed_enhanced.rds\n")
cat("  data-clean/proximity_fed_enhanced.csv\n")
cat("  data-clean/buffer_summary_stats.csv\n\n")

cat("Summary statistics:\n")
print(summary_stats)

# =============================================================================
# 7. Create visualization of geographic coverage
# =============================================================================
cat("\nCreating geographic coverage visualization...\n")

# Simple text-based visualization
coverage_plot <- function() {
  cat("\nGeographic Coverage of Indian Reserves by Federal Riding\n")
  cat("========================================================\n\n")
  
  for (dist_km in c(50, 100, 200)) {
    count_col <- paste0("reserves_within_", dist_km, "km")
    n_with <- sum(fed_prox[[count_col]] > 0)
    pct_with <- round(n_with / nrow(fed_prox) * 100, 1)
    
    cat(sprintf("%3d km buffer: %3d ridings (%5.1f%%) have ≥1 reserve\n", 
                dist_km, n_with, pct_with))
    
    # Create a simple histogram
    counts <- fed_prox[[count_col]]
    max_count <- min(10, max(counts))
    
    for (n in 0:max_count) {
      n_ridings <- sum(counts == n)
      pct <- round(n_ridings / nrow(fed_prox) * 100, 1)
      bar <- paste(rep("█", round(pct/2)), collapse = "")  # Scale for display
      cat(sprintf("  %2d reserves: %3d ridings (%5.1f%%) %s\n", 
                  n, n_ridings, pct, bar))
    }
    
    if (max(counts) > max_count) {
      n_gt <- sum(counts > max_count)
      pct_gt <- round(n_gt / nrow(fed_prox) * 100, 1)
      cat(sprintf("  >%d reserves: %3d ridings (%5.1f%%)\n", 
                  max_count, n_gt, pct_gt))
    }
    cat("\n")
  }
}

coverage_plot()

# Save coverage summary to text file
sink("data-clean/geographic_coverage_summary.txt")
coverage_plot()
sink()

cat("\nCoverage summary saved to: data-clean/geographic_coverage_summary.txt\n")