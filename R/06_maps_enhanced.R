# =============================================================================
# 06_maps_enhanced.R
# Enhanced maps:
#   Map A: Main Canada choropleth + 5 urban insets (Toronto, Montreal,
#           Vancouver, Calgary, Ottawa-Gatineau)
#   Map B: Reserves (points colored by CWB) overlaid on riding boundaries
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

.libPaths(c('/home/ral/R/library', .libPaths()))

library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(patchwork)
library(sf)
library(scales)

sf_use_s2(FALSE)
setwd("/home/ral/.openclaw/workspace/urban_proximity")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# THEME
# =============================================================================

theme_ajps_map <- function(base_size = 11) {
  theme_void(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size, hjust = 0,
                                     margin = margin(b = 3)),
      plot.subtitle   = element_text(size = base_size * 0.82, hjust = 0,
                                     color = "grey30", margin = margin(b = 4)),
      plot.caption    = element_text(size = base_size * 0.75, hjust = 1,
                                     color = "grey45", margin = margin(t = 5)),
      legend.position = "right",
      legend.title    = element_text(size = base_size * 0.82, face = "bold"),
      legend.text     = element_text(size = base_size * 0.75),
      legend.key.height = unit(1.1, "cm"),
      legend.key.width  = unit(0.38, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(6, 6, 6, 6)
    )
}

theme_ajps_inset <- function(base_size = 9) {
  theme_void(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size, hjust = 0.5,
                                     margin = margin(b = 2)),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(4, 4, 4, 4)
    )
}

save_fig <- function(p, name, w = 12, h = 8) {
  ggsave(paste0("output/figures/", name, ".pdf"), p, width = w, height = h, device = "pdf")
  ggsave(paste0("output/figures/", name, ".png"), p, width = w, height = h, dpi = 300)
  cat("Saved:", name, "\n")
}


# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading shapefiles...\n")

# Federal ridings (mainland only)
fed <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet = TRUE) %>%
  mutate(feduid = as.numeric(FEDUID), pruid = as.numeric(PRUID)) %>%
  filter(pruid < 60) %>%
  st_transform(3347)

# CMA boundaries (for inset boxes)
cma <- st_read("data/shapefiles/cma_boundary/lcma000b21a_e.shp", quiet = TRUE) %>%
  st_transform(3347)

# Expanded reserve dataset (1,021 communities incl. Cree/Naskapi, CWB 2011 imputed)
reserves <- readRDS("data-clean/reserves_expanded.rds") %>%
  st_transform(3347)

# Reserve centroids for point map
res_centroids <- st_centroid(reserves) %>%
  filter(!is.na(CWB))

cat("Reserves with CWB:", nrow(res_centroids), "\n")


# =============================================================================
# 2. BUILD RIDING-LEVEL DV
# =============================================================================

prox <- read.csv("data-clean/proximity_fed_v2.csv") %>%
  rename(feduid = FEDUID) %>%
  mutate(feduid = as.numeric(feduid))

ces <- readRDS("data/ces2021.rds") %>%
  mutate(
    therm_neg      = { raw <- zap_labels(cps21_groups_therm_4); raw[raw == -99] <- NA; (100 - raw) / 100 },
    groupdiscrim_neg = { x <- zap_labels(cps21_groupdiscrim_1); x[x == 6] <- NA; (x - 1) / 4 },
    ab_col_neg     = { x <- zap_labels(pes21_ab_col);     x[x == 6] <- NA; ((6 - x) - 1) / 4 },
    ab_deserve_neg = { x <- zap_labels(pes21_ab_deserve); x[x == 6] <- NA; ((6 - x) - 1) / 4 },
    ab_favors_neg  = { x <- zap_labels(pes21_ab_favors);  x[x == 6] <- NA; (x - 1) / 4 },
    feduid         = as.numeric(feduid),
    is_indigenous  = zap_labels(cps21_vismin_4) == 1,
    dv_perception  = rowMeans(cbind(therm_neg, groupdiscrim_neg, ab_favors_neg,
                                    ab_deserve_neg, ab_col_neg), na.rm = TRUE),
    dv_discrim     = rowMeans(cbind(groupdiscrim_neg, ab_deserve_neg, ab_col_neg), na.rm = TRUE)
  ) %>%
  mutate(across(starts_with("dv_"), ~ ifelse(is.nan(.), NA, .))) %>%
  filter(!is_indigenous)

riding_dv <- ces %>%
  group_by(feduid) %>%
  summarise(mean_perception = mean(dv_perception, na.rm = TRUE),
            mean_discrim    = mean(dv_discrim,    na.rm = TRUE),
            n_resp = n(), .groups = "drop") %>%
  mutate(mean_perception = ifelse(n_resp < 5, NA, mean_perception),
         mean_discrim    = ifelse(n_resp < 5, NA, mean_discrim))

fed_dv <- fed %>%
  left_join(riding_dv, by = "feduid") %>%
  left_join(prox %>% select(feduid, dist_nearest_cwb_Q4_km), by = "feduid")

# Shared fill scale limits (for consistent colour across main + insets)
perc_range <- range(fed_dv$mean_perception, na.rm = TRUE)
fill_limits <- c(floor(perc_range[1] * 20) / 20, ceiling(perc_range[2] * 20) / 20)

cat("Perception range:", round(fill_limits, 3), "\n")


# =============================================================================
# 3. DEFINE URBAN INSET CITIES
# =============================================================================

# CMA UIDs: Toronto=535, Montreal=462, Vancouver=933, Calgary=825, Ottawa-Gatineau=505
target_cmas <- tibble(
  CMAUID = c("535", "462", "933", "825", "505"),
  label  = c("Toronto", "Montreal", "Vancouver", "Calgary", "Ottawa-\nGatineau")
)

cma_sel <- cma %>%
  filter(CMAUID %in% target_cmas$CMAUID) %>%
  group_by(CMAUID) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  left_join(target_cmas, by = "CMAUID")

cat("Target CMAs found:", nrow(cma_sel), "\n")


# =============================================================================
# 4. HELPER: BUILD ONE INSET MAP
# =============================================================================

make_inset <- function(cma_row, fed_data, fill_lims, buffer_m = 30000) {
  cma_buf  <- st_buffer(cma_row, buffer_m)
  fed_clip <- st_filter(fed_data, cma_buf)
  bbox     <- st_bbox(cma_buf)

  ggplot() +
    geom_sf(data = fed_clip, aes(fill = mean_perception),
            color = "white", linewidth = 0.12) +
    geom_sf(data = cma_row, fill = NA, color = "black",
            linewidth = 0.5, linetype = "dashed") +
    scale_fill_gradientn(
      colors   = c("white", "grey65", "grey30", "black"),
      na.value = "grey85",
      limits   = fill_lims,
      guide    = "none"
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE) +
    labs(title = cma_row$label) +
    theme_ajps_inset()
}


# =============================================================================
# 5. BUILD MAIN MAP + INSETS  (MAP A)
# =============================================================================

cat("Building Map A (main + insets)...\n")

# Main map
main_map <- ggplot(fed_dv) +
  geom_sf(aes(fill = mean_perception), color = "white", linewidth = 0.06) +
  scale_fill_gradientn(
    colors   = c("white", "grey65", "grey30", "black"),
    na.value = "grey88",
    name     = "Mean\nnegative\nperception\n(0-1)",
    labels   = label_number(accuracy = 0.01),
    limits   = fill_limits,
    guide    = guide_colorbar(barheight = unit(4.5, "cm"), barwidth = unit(0.4, "cm"),
                              ticks.colour = "black", frame.colour = "black")
  ) +
  labs(
    title    = "Figure 4. Mean Negative Perception of Indigenous Peoples by Federal Riding",
    subtitle = "CES 2021, non-Indigenous respondents | Higher = more negative | Dashed boxes = urban inset areas",
    caption  = "Scale: mean of 5 items, 0-1. Grey = <5 respondents. Federal ridings 2021, territories excluded."
  ) +
  theme_ajps_map()

# Build 5 insets
insets <- lapply(seq_len(nrow(cma_sel)), function(i) {
  make_inset(cma_sel[i, ], fed_dv, fill_limits)
})

# Add dashed boxes on main map showing inset locations
main_map_with_boxes <- main_map +
  geom_sf(data = cma_sel, fill = NA, color = "black",
          linewidth = 0.5, linetype = "dashed")

# Compose: main map + 5 insets in a row below
inset_row <- wrap_plots(insets, nrow = 1)

fig_main_insets <- main_map_with_boxes / inset_row +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

save_fig(fig_main_insets, "fig4_map_with_insets", w = 14, h = 11)


# =============================================================================
# 6. MAP B — Reserves (CWB color) + Riding boundaries
# =============================================================================

cat("Building Map B (reserves + ridings)...\n")

cwb_breaks <- c(30, 45, 55, 65, 75, 90)

fig_reserves <- ggplot() +
  # Riding boundaries (light grey fill, thin border)
  geom_sf(data = fed_dv,
          fill = "grey96", color = "grey70", linewidth = 0.12) +
  # Reserve polygons colored by CWB (small, so use fill with outline)
  geom_sf(data = reserves %>% filter(!is.na(CWB)) %>% st_transform(3347),
          aes(fill = CWB), color = NA) +
  # Reserve centroids as points for very small reserves
  geom_sf(data = res_centroids %>% filter(!is.na(Pop), Pop < 200),
          aes(color = CWB), size = 0.8, shape = 16, alpha = 0.85) +
  scale_fill_gradientn(
    colors  = c("black", "grey40", "grey70", "white"),
    limits  = c(30, 90),
    breaks  = cwb_breaks,
    labels  = cwb_breaks,
    name    = "CWB Index\n(2016)",
    guide   = guide_colorbar(barheight = unit(4.5, "cm"), barwidth = unit(0.4, "cm"),
                             ticks.colour = "black", frame.colour = "black",
                             title.position = "top")
  ) +
  scale_color_gradientn(
    colors  = c("black", "grey40", "grey70", "white"),
    limits  = c(30, 90),
    guide   = "none"
  ) +
  labs(
    title    = "Figure 6. Indian Reserves and Federal Electoral Ridings in Canada",
    subtitle = "Reserve fill: Community Well-Being Index (darker = lower well-being). Riding boundaries in grey.",
    caption  = paste0("N = ", nrow(res_centroids), " reserves with CWB data.",
                      " CWB: Statistics Canada 2016. Federal ridings 2021, territories excluded.")
  ) +
  theme_ajps_map()

save_fig(fig_reserves, "fig6_map_reserves_cwb", w = 13, h = 8)


# =============================================================================
# 7. BONUS — Reserves map with urban insets (same 5 cities)
# =============================================================================

cat("Building Map B with insets...\n")

make_reserve_inset <- function(cma_row, fed_data, res_data, res_pts, buffer_m = 30000) {
  cma_buf  <- st_buffer(cma_row, buffer_m)
  fed_clip <- st_filter(fed_data, cma_buf)
  res_clip <- suppressWarnings(st_filter(res_data %>% filter(!is.na(CWB)), cma_buf))
  pts_clip <- suppressWarnings(st_filter(res_pts  %>% filter(!is.na(CWB)), cma_buf))
  bbox     <- st_bbox(cma_buf)

  ggplot() +
    geom_sf(data = fed_clip, fill = "grey96", color = "grey65", linewidth = 0.15) +
    geom_sf(data = res_clip, aes(fill = CWB), color = NA) +
    geom_sf(data = pts_clip, aes(color = CWB), size = 1.2, shape = 16) +
    geom_sf(data = cma_row, fill = NA, color = "black",
            linewidth = 0.5, linetype = "dashed") +
    scale_fill_gradientn(colors = c("black","grey40","grey70","white"),
                         limits = c(30, 90), guide = "none") +
    scale_color_gradientn(colors = c("black","grey40","grey70","white"),
                          limits = c(30, 90), guide = "none") +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    labs(title = cma_row$label) +
    theme_ajps_inset()
}

res_poly_proj <- reserves %>% filter(!is.na(CWB)) %>% st_transform(3347)
res_pts_proj  <- res_centroids %>% filter(!is.na(CWB)) %>% st_transform(3347)

reserve_insets <- lapply(seq_len(nrow(cma_sel)), function(i) {
  tryCatch(
    make_reserve_inset(cma_sel[i, ], fed_dv, res_poly_proj, res_pts_proj),
    error = function(e) { cat("Inset error:", cma_sel$label[i], "-", e$message, "\n"); NULL }
  )
})
reserve_insets <- Filter(Negate(is.null), reserve_insets)

fig_reserves_with_boxes <- fig_reserves +
  geom_sf(data = cma_sel, fill = NA, color = "black",
          linewidth = 0.5, linetype = "dashed")

reserve_inset_row <- wrap_plots(reserve_insets, nrow = 1)

fig_reserves_insets <- fig_reserves_with_boxes / reserve_inset_row +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

save_fig(fig_reserves_insets, "fig6_map_reserves_cwb_insets", w = 14, h = 11)

cat("\n=== DONE ===\n")
cat(list.files("output/figures", pattern = "fig4_map_with|fig6_map", full.names = FALSE), sep = "\n")
