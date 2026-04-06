# =============================================================================
# 05_maps.R
# Choropleth maps — AJPS black-and-white style
# Fixes: s2 disabled, correct SC Lambert bbox, filter territories
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# =============================================================================

.libPaths(c('/home/ral/R/library', .libPaths()))

library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(sf)
library(scales)

sf_use_s2(FALSE)   # avoid topology errors in StatCan shapefiles

setwd("/home/ral/.openclaw/workspace/urban_proximity")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# AJPS MAP THEME
# =============================================================================

theme_ajps_map <- function(base_size = 12) {
  theme_void(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size * 1.05, hjust = 0, margin = margin(b = 4)),
      plot.subtitle   = element_text(size = base_size * 0.88, hjust = 0, color = "grey30", margin = margin(b = 6)),
      plot.caption    = element_text(size = base_size * 0.78, hjust = 1, color = "grey45", margin = margin(t = 6)),
      legend.position = "right",
      legend.title    = element_text(size = base_size * 0.85, face = "bold"),
      legend.text     = element_text(size = base_size * 0.78),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width  = unit(0.4, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(12, 12, 12, 12)
    )
}

save_fig <- function(p, name, w = 11, h = 7) {
  ggsave(paste0("output/figures/", name, ".pdf"), p, width = w, height = h, device = "pdf")
  ggsave(paste0("output/figures/", name, ".png"), p, width = w, height = h, dpi = 300)
  cat("Saved:", name, "\n")
}


# =============================================================================
# 1. LOAD & PREPARE SHAPEFILES
# =============================================================================

# Federal ridings — exclude territories (PRUID 60, 61, 62)
fed <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet = TRUE) %>%
  mutate(feduid = as.numeric(FEDUID),
         pruid  = as.numeric(PRUID)) %>%
  filter(pruid < 60) %>%                     # drop Yukon / NWT / Nunavut
  st_transform(3347)                          # Statistics Canada Lambert

cat("Federal ridings (mainland):", nrow(fed), "\n")
cat("Bbox:", paste(round(st_bbox(fed)), collapse = ", "), "\n")


# =============================================================================
# 2. BUILD RIDING-LEVEL DV DATA
# =============================================================================

prox <- read.csv("data-clean/proximity_fed_v2.csv") %>%
  rename(feduid = FEDUID) %>%
  mutate(feduid = as.numeric(feduid))

ces <- readRDS("data/ces2021.rds") %>%
  mutate(
    therm_neg        = { raw <- zap_labels(cps21_groups_therm_4); raw[raw == -99] <- NA; (100 - raw) / 100 },
    groupdiscrim_neg = { x <- zap_labels(cps21_groupdiscrim_1); x[x == 6] <- NA; (x - 1) / 4 },
    ab_col_neg       = { x <- zap_labels(pes21_ab_col);     x[x == 6] <- NA; ((6 - x) - 1) / 4 },
    ab_deserve_neg   = { x <- zap_labels(pes21_ab_deserve); x[x == 6] <- NA; ((6 - x) - 1) / 4 },
    ab_favors_neg    = { x <- zap_labels(pes21_ab_favors);  x[x == 6] <- NA; (x - 1) / 4 },
    feduid       = as.numeric(feduid),
    is_indigenous = zap_labels(cps21_vismin_4) == 1,
    dv_perception = rowMeans(cbind(therm_neg, groupdiscrim_neg, ab_favors_neg, ab_deserve_neg, ab_col_neg), na.rm = TRUE),
    dv_discrim    = rowMeans(cbind(groupdiscrim_neg, ab_deserve_neg, ab_col_neg), na.rm = TRUE)
  ) %>%
  mutate(across(starts_with("dv_"), ~ ifelse(is.nan(.), NA, .))) %>%
  filter(!is_indigenous)

riding_dv <- ces %>%
  group_by(feduid) %>%
  summarise(
    mean_perception = mean(dv_perception, na.rm = TRUE),
    mean_discrim    = mean(dv_discrim,    na.rm = TRUE),
    n_resp          = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mean_perception = ifelse(n_resp < 5, NA, mean_perception),
    mean_discrim    = ifelse(n_resp < 5, NA, mean_discrim)
  )

cat("Riding DV data: ", nrow(riding_dv), "ridings\n")
cat("Range perception:", round(range(riding_dv$mean_perception, na.rm=TRUE), 3), "\n")


# =============================================================================
# 3. JOIN DATA TO SHAPEFILE
# =============================================================================

fed_dv <- fed %>%
  left_join(riding_dv, by = "feduid") %>%
  left_join(
    prox %>% select(feduid, dist_nearest_cwb_Q4_km, log_dist_cwb_q4_km,
                    nearest_reserve_cwb, dist_nearest_reserve_km),
    by = "feduid"
  )

cat("Joined shapefile rows:", nrow(fed_dv), "\n")
cat("Non-NA perception values:", sum(!is.na(fed_dv$mean_perception)), "\n")
cat("Non-NA CWB Q4 dist values:", sum(!is.na(fed_dv$dist_nearest_cwb_Q4_km)), "\n")


# =============================================================================
# 4. MAP A — Mean Negative Perception by Federal Riding
# =============================================================================

fig_map_a <- ggplot(fed_dv) +
  geom_sf(aes(fill = mean_perception), color = "white", linewidth = 0.07) +
  scale_fill_gradientn(
    colors   = c("white", "grey65", "grey30", "black"),
    na.value = "grey88",
    name     = "Mean\nnegative\nperception\n(0–1)",
    labels   = label_number(accuracy = 0.01),
    limits   = c(
      floor(min(fed_dv$mean_perception, na.rm = TRUE) * 20) / 20,
      ceiling(max(fed_dv$mean_perception, na.rm = TRUE) * 20) / 20
    ),
    guide    = guide_colorbar(
      barheight = unit(5, "cm"),
      barwidth  = unit(0.45, "cm"),
      ticks.colour = "black",
      frame.colour = "black"
    )
  ) +
  labs(
    title    = "Figure 4. Mean Negative Perception of Indigenous Peoples by Federal Riding",
    subtitle = "Higher values = more negative perception of Indigenous peoples (CES 2021, non-Indigenous respondents)",
    caption  = "Scale: mean of 5 items rescaled 0–1. Grey = <5 CES respondents in riding.\nFederal ridings, 2021 boundaries. Territories excluded."
  ) +
  theme_ajps_map()

save_fig(fig_map_a, "fig4_map_dv_by_riding")


# =============================================================================
# 5. MAP B — Distance to Nearest High-CWB Reserve
# =============================================================================

fed_dv <- fed_dv %>%
  mutate(dist_cwb_q4_display = dist_nearest_cwb_Q4_km)

fig_map_b <- ggplot(fed_dv) +
  geom_sf(aes(fill = dist_cwb_q4_display), color = "white", linewidth = 0.07) +
  scale_fill_gradientn(
    colors   = c("black", "grey35", "grey70", "white"),
    na.value = "grey88",
    name     = "Distance to\nhigh-CWB\nreserve (km)",
    labels   = label_comma(),
    trans    = "log10",
    guide    = guide_colorbar(
      barheight = unit(5, "cm"),
      barwidth  = unit(0.45, "cm"),
      ticks.colour = "black",
      frame.colour = "black"
    )
  ) +
  labs(
    title    = "Figure 5. Distance to Nearest High-CWB Reserve by Federal Riding",
    subtitle = "Darker = closer to a prosperous (top-quartile Community Well-Being Index) reserve",
    caption  = "CWB: Community Well-Being Index 2016, Statistics Canada. Distance = riding centroid to reserve centroid.\nFederal ridings, 2021 boundaries. Territories excluded."
  ) +
  theme_ajps_map()

save_fig(fig_map_b, "fig5_map_highcwb_proximity")


# =============================================================================
# 6. MAP C — Discrimination Perception (sub-scale) by Riding
# =============================================================================

fig_map_c <- ggplot(fed_dv) +
  geom_sf(aes(fill = mean_discrim), color = "white", linewidth = 0.07) +
  scale_fill_gradientn(
    colors   = c("white", "grey65", "grey30", "black"),
    na.value = "grey88",
    name     = "Mean\ndiscrimination\ndenial\n(0–1)",
    labels   = label_number(accuracy = 0.01),
    guide    = guide_colorbar(
      barheight = unit(5, "cm"),
      barwidth  = unit(0.45, "cm"),
      ticks.colour = "black",
      frame.colour = "black"
    )
  ) +
  labs(
    title    = "Figure 4b. Mean Discrimination Perception (Denial) by Federal Riding",
    subtitle = "Higher = greater denial that Indigenous peoples face discrimination (3-item sub-scale)",
    caption  = "CES 2021. Items: groupdiscrim_neg, ab_deserve_neg, ab_col_neg. Grey = <5 respondents."
  ) +
  theme_ajps_map()

save_fig(fig_map_c, "fig4b_map_discrim_by_riding")


# =============================================================================
# 7. BONUS: SIDE-BY-SIDE COMPARISON MAP
# =============================================================================

library(patchwork)

fig_combined <- (fig_map_a + theme(plot.title = element_text(size = 10),
                                    plot.subtitle = element_text(size = 8))) /
                (fig_map_b + theme(plot.title = element_text(size = 10),
                                    plot.subtitle = element_text(size = 8))) +
  plot_annotation(
    title   = "Perception of Indigenous Peoples and Proximity to Prosperous Reserves",
    subtitle = "Federal Electoral Districts, Canada (territories excluded)",
    caption  = "CES 2021. Community Well-Being Index 2016, Statistics Canada.",
    theme   = theme(
      plot.title    = element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = element_text(size = 9, color = "grey30"),
      plot.caption  = element_text(size = 8, color = "grey45", hjust = 1),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

save_fig(fig_combined, "fig_maps_combined", w = 11, h = 12)

cat("\n=== MAPS DONE ===\n")
cat(list.files("output/figures", pattern = "fig4|fig5|fig_maps", full.names = FALSE), sep = "\n")
