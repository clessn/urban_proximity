# =============================================================================
# 04_analysis_and_viz.R
# Full analysis pipeline: scale construction → clustered SE models → figures
#
# Project: Urban Proximity & Indigenous Attitudes
# Authors: Laurence-Olivier M. Foisy, Camille Pelletier
# Style: AJPS black-and-white professional
# =============================================================================

.libPaths(c('/home/ral/R/library', .libPaths()))

library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(patchwork)
library(scales)
library(ggrepel)
library(sf)
library(lmtest)
library(sandwich)
library(broom)
library(fixest)

setwd("/home/ral/.openclaw/workspace/urban_proximity")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# AJPS THEME
# =============================================================================

theme_ajps <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      # Panel
      panel.grid.major   = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.6),
      panel.background   = element_rect(fill = "white"),
      # Axes
      axis.line          = element_blank(),
      axis.ticks         = element_line(color = "black", linewidth = 0.4),
      axis.text          = element_text(color = "black", size = base_size * 0.85),
      axis.title         = element_text(color = "black", size = base_size, face = "plain"),
      # Legend
      legend.background  = element_rect(fill = "white", color = "black", linewidth = 0.3),
      legend.key         = element_rect(fill = "white"),
      legend.title       = element_text(size = base_size * 0.85, face = "bold"),
      legend.text        = element_text(size = base_size * 0.8),
      legend.position    = "bottom",
      # Strips
      strip.background   = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
      strip.text         = element_text(size = base_size * 0.85, face = "bold"),
      # Title / caption
      plot.title         = element_text(size = base_size * 1.1, face = "bold", hjust = 0),
      plot.subtitle      = element_text(size = base_size * 0.9, hjust = 0, color = "grey30"),
      plot.caption       = element_text(size = base_size * 0.75, hjust = 1, color = "grey40"),
      plot.margin        = margin(10, 15, 10, 10)
    )
}

save_fig <- function(p, name, w = 8, h = 5.5) {
  path <- paste0("output/figures/", name, ".pdf")
  ggsave(path, p, width = w, height = h, device = "pdf")
  path_png <- paste0("output/figures/", name, ".png")
  ggsave(path_png, p, width = w, height = h, dpi = 300)
  cat("Saved:", path, "\n")
}


# =============================================================================
# 1. BUILD ANALYSIS DATASET
# =============================================================================

prox <- read.csv("data-clean/proximity_fed_enhanced.csv") %>%
  rename(feduid = FEDUID) %>%
  mutate(feduid = as.numeric(feduid))

ces <- readRDS("data/ces2021.rds") %>%
  mutate(
    therm_neg        = { raw <- zap_labels(cps21_groups_therm_4); raw[raw == -99] <- NA; (100 - raw) / 100 },
    groupdiscrim_neg = { x <- zap_labels(cps21_groupdiscrim_1); x[x == 6] <- NA; (x - 1) / 4 },
    ab_col_neg       = { x <- zap_labels(pes21_ab_col);     x[x == 6] <- NA; ((6 - x) - 1) / 4 },
    ab_deserve_neg   = { x <- zap_labels(pes21_ab_deserve); x[x == 6] <- NA; ((6 - x) - 1) / 4 },
    ab_favors_neg    = { x <- zap_labels(pes21_ab_favors);  x[x == 6] <- NA; (x - 1) / 4 },
    age          = as.numeric(cps21_age),
    female       = ifelse(zap_labels(cps21_genderid) == 2, 1, ifelse(zap_labels(cps21_genderid) == 1, 0, NA)),
    educ         = ifelse(zap_labels(cps21_education) == 12, NA, zap_labels(cps21_education)),
    born_canada  = ifelse(zap_labels(cps21_bornin_canada) == 1, 1, ifelse(zap_labels(cps21_bornin_canada) == 2, 0, NA)),
    urban        = ifelse(zap_labels(pes21_rural_urban) %in% 1:5, zap_labels(pes21_rural_urban), NA),
    urban_binary = ifelse(urban %in% 4:5, 1, ifelse(urban %in% 1:3, 0, NA)),
    province     = zap_labels(cps21_province),
    feduid       = as.numeric(feduid),
    is_indigenous = zap_labels(cps21_vismin_4) == 1,
    dv_affect      = therm_neg,
    dv_discrim     = rowMeans(cbind(groupdiscrim_neg, ab_deserve_neg, ab_col_neg), na.rm = TRUE),
    dv_resentment  = rowMeans(cbind(ab_favors_neg, ab_deserve_neg, ab_col_neg), na.rm = TRUE),
    dv_perception  = rowMeans(cbind(therm_neg, groupdiscrim_neg, ab_favors_neg, ab_deserve_neg, ab_col_neg), na.rm = TRUE)
  ) %>%
  mutate(across(starts_with("dv_"), ~ ifelse(is.nan(.), NA, .))) %>%
  filter(!is_indigenous) %>%
  left_join(
    prox %>% select(feduid, dist_nearest_reserve_km, log_dist_nearest_km,
                    log_reserves_100km, reserves_within_100km,
                    log_dist_cwb_q4_km, log_dist_cwb_q1_km,
                    nearest_reserve_cwb, nearest_reserve_urban,
                    dist_nearest_cwb_Q4_highest_km),
    by = "feduid"
  )

cat("Analysis dataset:", nrow(ces), "respondents\n")


# =============================================================================
# 2. MODELS WITH CLUSTERED STANDARD ERRORS (by federal riding)
# =============================================================================

ctrl <- "age + female + educ + born_canada"

# Helper: run OLS + clustered SEs by feduid, return tidy output
run_clustered <- function(dv, iv_extra = "", label = "") {
  fml <- as.formula(paste(dv, "~", iv_extra, "+", ctrl, "+ factor(province)"))
  m   <- lm(fml, data = ces)
  cl  <- coeftest(m, vcov = vcovCL(m, cluster = ~ feduid))
  td  <- tidy(cl) %>%
    filter(!grepl("province|Intercept", term)) %>%
    mutate(model = label, dv = dv,
           conf.low  = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
  list(model = m, coefs = td, n = nobs(m), r2 = summary(m)$r.squared)
}

# --- Part 1: Urban effect on each DV ---
p1a <- run_clustered("dv_affect",     "urban_binary", "Affect (thermometer)")
p1b <- run_clustered("dv_discrim",    "urban_binary", "Discrimination perception")
p1c <- run_clustered("dv_resentment", "urban_binary", "Resentment scale")

# --- Part 2: Urban × High-CWB proximity interaction ---
p2a <- run_clustered("dv_affect",     "log_dist_cwb_q4_km * urban_binary", "Affect")
p2b <- run_clustered("dv_discrim",    "log_dist_cwb_q4_km * urban_binary", "Discrimination perception")
p2c <- run_clustered("dv_resentment", "log_dist_cwb_q4_km * urban_binary", "Resentment scale")
p2d <- run_clustered("dv_perception", "log_dist_cwb_q4_km * urban_binary", "Full perception scale")

cat("\n=== PART 1: Urban effect (clustered SEs) ===\n")
for (r in list(p1a, p1b, p1c)) {
  cat("\nDV:", r$coefs$dv[1], "| N:", r$n, "| R²:", round(r$r2, 3), "\n")
  print(r$coefs %>% select(term, estimate, std.error, statistic, p.value) %>%
          mutate(across(where(is.numeric), ~ round(., 4))))
}

cat("\n=== PART 2: Urban × High-CWB proximity (clustered SEs) ===\n")
for (r in list(p2a, p2b, p2c, p2d)) {
  cat("\nDV:", r$coefs$dv[1], "| N:", r$n, "| R²:", round(r$r2, 3), "\n")
  print(r$coefs %>% select(term, estimate, std.error, statistic, p.value) %>%
          mutate(across(where(is.numeric), ~ round(., 4))))
}


# =============================================================================
# 3. FIGURE 1 — Urban effect on perception dimensions (coefficient plot)
# =============================================================================

urban_coefs <- bind_rows(
  p1a$coefs %>% filter(term == "urban_binary"),
  p1b$coefs %>% filter(term == "urban_binary"),
  p1c$coefs %>% filter(term == "urban_binary")
) %>%
  mutate(
    model = factor(model, levels = c("Affect (thermometer)", "Discrimination perception", "Resentment scale")),
    sig   = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05")
  )

fig1 <- ggplot(urban_coefs, aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, linewidth = 0.7) +
  geom_point(aes(shape = sig), size = 3.5, fill = "white") +
  scale_shape_manual(values = c("p < 0.05" = 19, "p ≥ 0.05" = 1),
                     name = "Significance") +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Figure 1. Effect of Urban Residence on Negative Perception of Indigenous Peoples",
    subtitle = "OLS with clustered SEs (by federal riding); controls: age, gender, education, born in Canada, province FE",
    x        = "Coefficient estimate (95% CI)\nDV scaled 0–1; higher = more negative perception",
    y        = NULL,
    caption  = "CES 2021. Non-Indigenous respondents only. N ≈ 13,000–14,000."
  ) +
  theme_ajps()

save_fig(fig1, "fig1_urban_effect", w = 8, h = 4)


# =============================================================================
# 4. FIGURE 2 — Interaction plot: Urban × proximity to high-CWB reserve
# =============================================================================

# Marginal predictions: vary log_dist_cwb_q4_km at urban=0 and urban=1
pred_data <- expand.grid(
  log_dist_cwb_q4_km = seq(
    quantile(ces$log_dist_cwb_q4_km, 0.05, na.rm = TRUE),
    quantile(ces$log_dist_cwb_q4_km, 0.95, na.rm = TRUE),
    length.out = 50
  ),
  urban_binary = c(0, 1),
  age          = mean(ces$age, na.rm = TRUE),
  female       = 0.5,
  educ         = mean(ces$educ, na.rm = TRUE),
  born_canada  = mean(ces$born_canada, na.rm = TRUE)
)

# Add modal province
pred_data$province <- as.numeric(names(sort(table(ces$province), decreasing = TRUE)[1]))

make_pred_plot <- function(model_obj, dv_label, fig_label) {
  pred <- predict(model_obj, newdata = pred_data, interval = "confidence")
  pred_df <- cbind(pred_data, as.data.frame(pred)) %>%
    mutate(urban_label = ifelse(urban_binary == 1, "Urban (suburb/large city)", "Non-urban (rural/small town)"))

  # Convert log_dist back to km for x-axis label
  pred_df$dist_km <- exp(pred_df$log_dist_cwb_q4_km)

  ggplot(pred_df, aes(x = dist_km, y = fit,
                      linetype = urban_label, group = urban_label)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, fill = "grey40") +
    geom_line(linewidth = 0.9) +
    scale_x_log10(labels = label_comma(suffix = " km")) +
    scale_y_continuous(limits = c(0.2, 0.7), labels = label_number(accuracy = 0.01)) +
    scale_linetype_manual(values = c("Urban (suburb/large city)" = "solid",
                                     "Non-urban (rural/small town)" = "dashed"),
                          name = NULL) +
    labs(
      title    = fig_label,
      subtitle = "Marginal predictions at mean controls; province modal category",
      x        = "Distance to nearest high-CWB reserve (km, log scale)",
      y        = paste0("Predicted ", dv_label, "\n(0 = most positive, 1 = most negative)"),
      caption  = "CES 2021. 95% confidence bands."
    ) +
    theme_ajps() +
    theme(legend.position = "bottom")
}

fig2a <- make_pred_plot(p2b$model, "discrimination perception",
                        "Figure 2a. Distance to Prosperous Reserve × Urban: Discrimination Perception")
fig2b <- make_pred_plot(p2c$model, "resentment scale",
                        "Figure 2b. Distance to Prosperous Reserve × Urban: Resentment Scale")
fig2c <- make_pred_plot(p2a$model, "affect (thermometer)",
                        "Figure 2c. Distance to Prosperous Reserve × Urban: Thermometer")

save_fig(fig2a, "fig2a_interaction_discrim",  w = 8, h = 5.5)
save_fig(fig2b, "fig2b_interaction_resentment", w = 8, h = 5.5)
save_fig(fig2c, "fig2c_interaction_affect",   w = 8, h = 5.5)


# =============================================================================
# 5. FIGURE 3 — Coefficient plot: interaction terms across DVs
# =============================================================================

interaction_coefs <- bind_rows(
  p2a$coefs %>% filter(grepl(":", term)) %>% mutate(dv_label = "Affect\n(thermometer)"),
  p2b$coefs %>% filter(grepl(":", term)) %>% mutate(dv_label = "Discrimination\nperception"),
  p2c$coefs %>% filter(grepl(":", term)) %>% mutate(dv_label = "Resentment\nscale"),
  p2d$coefs %>% filter(grepl(":", term)) %>% mutate(dv_label = "Full perception\nscale")
) %>%
  mutate(
    dv_label = factor(dv_label, levels = c("Affect\n(thermometer)", "Discrimination\nperception",
                                           "Resentment\nscale", "Full perception\nscale")),
    sig = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05")
  )

fig3 <- ggplot(interaction_coefs, aes(x = estimate, y = dv_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 0.7) +
  geom_point(aes(shape = sig), size = 4, fill = "white") +
  scale_shape_manual(values = c("p < 0.05" = 19, "p ≥ 0.05" = 1), name = "Significance") +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  labs(
    title    = "Figure 3. Interaction Effect: Urban × Proximity to High-CWB Reserve",
    subtitle = "Negative coefficient = urban respondents closer to prosperous reserve have more negative perception\nOLS with clustered SEs (by riding); province FE; controls: age, gender, education, born in Canada",
    x        = "Interaction coefficient estimate (95% CI)",
    y        = NULL,
    caption  = "CES 2021. Non-Indigenous respondents only."
  ) +
  theme_ajps()

save_fig(fig3, "fig3_interaction_coefs", w = 8, h = 4.5)


# =============================================================================
# 6. FIGURE 4 — Map: DV by federal riding
# =============================================================================

# Riding-level mean DV
riding_dv <- ces %>%
  group_by(feduid) %>%
  summarise(
    mean_perception = mean(dv_perception, na.rm = TRUE),
    mean_discrim    = mean(dv_discrim, na.rm = TRUE),
    n_resp          = n(),
    .groups         = "drop"
  ) %>%
  filter(n_resp >= 5)

fed_shp <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet = TRUE) %>%
  mutate(feduid = as.numeric(FEDUID)) %>%
  left_join(riding_dv, by = "feduid") %>%
  st_transform(3347)  # Statistics Canada Lambert

# Mainland Canada bounding box (exclude far north)
bbox_main <- st_bbox(c(xmin = -2400000, ymin = 500000, xmax = 3000000, ymax = 3500000),
                     crs = st_crs(3347))

fed_main <- st_crop(fed_shp, bbox_main)

fig4 <- ggplot(fed_main) +
  geom_sf(aes(fill = mean_perception), color = "white", linewidth = 0.05) +
  scale_fill_gradientn(
    colors   = c("white", "grey60", "grey20", "black"),
    na.value = "grey90",
    name     = "Mean negative\nperception (0–1)",
    labels   = label_number(accuracy = 0.01),
    limits   = c(0.2, 0.6)
  ) +
  labs(
    title   = "Figure 4. Mean Negative Perception of Indigenous Peoples by Federal Riding",
    subtitle = "Higher values = more negative perception; CES 2021 respondents (non-Indigenous)",
    caption  = "Scale: mean of 5 items, each 0–1; grey = insufficient respondents (<5)."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, hjust = 0, color = "grey30"),
    plot.caption  = element_text(size = 8, hjust = 1, color = "grey40"),
    legend.position = "right",
    legend.title  = element_text(size = 9),
    legend.text   = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin   = margin(10, 10, 10, 10)
  )

save_fig(fig4, "fig4_map_dv_by_riding", w = 10, h = 6.5)


# =============================================================================
# 7. FIGURE 5 — Map: proximity to high-CWB reserve by riding
# =============================================================================

prox_map <- prox %>%
  select(feduid, dist_nearest_cwb_Q4_highest_km, nearest_reserve_cwb) %>%
  mutate(feduid = as.numeric(feduid))

fed_prox <- st_read("data/shapefiles/fed_boundary/lfed000b21a_e.shp", quiet = TRUE) %>%
  mutate(feduid = as.numeric(FEDUID)) %>%
  left_join(prox_map, by = "feduid") %>%
  st_transform(3347)

fed_prox_main <- st_crop(fed_prox, bbox_main)

fig5 <- ggplot(fed_prox_main) +
  geom_sf(aes(fill = dist_nearest_cwb_Q4_highest_km), color = "white", linewidth = 0.05) +
  scale_fill_gradientn(
    colors   = c("black", "grey40", "grey75", "white"),
    na.value = "grey90",
    name     = "Distance to\nhigh-CWB reserve (km)",
    labels   = label_comma(),
    trans    = "log10"
  ) +
  labs(
    title    = "Figure 5. Distance to Nearest High-CWB Reserve by Federal Riding",
    subtitle = "Darker = closer to a prosperous (top-quartile CWB) reserve",
    caption  = "CWB: Community Well-Being Index, Statistics Canada. Log scale."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, hjust = 0, color = "grey30"),
    plot.caption  = element_text(size = 8, hjust = 1, color = "grey40"),
    legend.position = "right",
    legend.title  = element_text(size = 9),
    legend.text   = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin   = margin(10, 10, 10, 10)
  )

save_fig(fig5, "fig5_map_highcwb_proximity", w = 10, h = 6.5)


# =============================================================================
# 8. FIGURE 6 — Scatter: riding-level DV vs proximity to high-CWB reserve
#              (urban vs non-urban split panel)
# =============================================================================

riding_scatter <- ces %>%
  group_by(feduid) %>%
  summarise(
    mean_discrim     = mean(dv_discrim, na.rm = TRUE),
    mean_resentment  = mean(dv_resentment, na.rm = TRUE),
    prop_urban       = mean(urban_binary, na.rm = TRUE),
    n_resp           = n(),
    .groups          = "drop"
  ) %>%
  filter(n_resp >= 10) %>%
  left_join(prox %>% select(feduid, dist_nearest_cwb_Q4_highest_km, log_dist_cwb_q4_km) %>%
              mutate(feduid = as.numeric(feduid)), by = "feduid") %>%
  mutate(urban_cat = ifelse(prop_urban >= 0.5, "Majority urban ridings", "Majority non-urban ridings"))

fig6 <- ggplot(riding_scatter %>% filter(!is.na(urban_cat)),
               aes(x = exp(log_dist_cwb_q4_km), y = mean_discrim)) +
  geom_point(size = 1.5, alpha = 0.6, shape = 16, color = "grey30") +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey70",
              linewidth = 0.8, linetype = "solid") +
  facet_wrap(~ urban_cat) +
  scale_x_log10(labels = label_comma(suffix = " km")) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Figure 6. Riding-Level Discrimination Perception vs. Distance to High-CWB Reserve",
    subtitle = "Each point = one federal riding (N ≥ 10 CES respondents); linear fit with 95% CI",
    x        = "Distance to nearest high-CWB reserve (km, log scale)",
    y        = "Mean discrimination perception (0–1)",
    caption  = "CES 2021. Urban classification based on proportion of respondents in suburban/urban areas."
  ) +
  theme_ajps()

save_fig(fig6, "fig6_scatter_riding_discrim", w = 9, h = 5)


# =============================================================================
# 9. FIGURE 7 — Distribution of DV items (density plots)
# =============================================================================

item_long <- ces %>%
  select(therm_neg, groupdiscrim_neg, ab_favors_neg, ab_deserve_neg, ab_col_neg) %>%
  pivot_longer(everything(), names_to = "item", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(item = recode(item,
    "therm_neg"        = "Thermometer\n(reversed)",
    "groupdiscrim_neg" = "Perceived\ndiscrimination",
    "ab_favors_neg"    = "Resentment:\nBootstrap",
    "ab_deserve_neg"   = "Resentment:\nDeserved less",
    "ab_col_neg"       = "Resentment:\nColonialism"
  ))

fig7 <- ggplot(item_long, aes(x = value)) +
  geom_histogram(bins = 20, fill = "grey30", color = "white", linewidth = 0.2) +
  facet_wrap(~ item, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  labs(
    title    = "Figure 7. Distribution of Scale Items",
    subtitle = "All items rescaled 0–1; higher = more negative perception of Indigenous peoples",
    x        = "Item value",
    y        = "Count",
    caption  = "CES 2021. Non-Indigenous respondents only."
  ) +
  theme_ajps(base_size = 10)

save_fig(fig7, "fig7_item_distributions", w = 12, h = 4)


# =============================================================================
# 10. FIGURE 8 — Factor loadings bar chart
# =============================================================================

fa_items <- ces %>%
  select(therm_neg, groupdiscrim_neg, ab_favors_neg, ab_deserve_neg, ab_col_neg) %>%
  drop_na()

fa_res <- factanal(fa_items, factors = 1)
loadings_df <- data.frame(
  item    = c("Thermometer\n(reversed)", "Perceived\ndiscrimination",
              "Resentment:\nBootstrap", "Resentment:\nDeserved less",
              "Resentment:\nColonialism"),
  loading = as.numeric(fa_res$loadings[, 1])
) %>% arrange(loading) %>%
  mutate(item = factor(item, levels = item))

alpha_val <- round(psych::alpha(fa_items)$total$raw_alpha, 3)
eigen_val <- round(eigen(cor(fa_items))$values[1], 3)

fig8 <- ggplot(loadings_df, aes(x = loading, y = item)) +
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_col(fill = "grey25", width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", loading)), hjust = -0.15, size = 3.5) +
  scale_x_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +
  annotate("text", x = 0.85, y = 1.3,
           label = paste0("α = ", alpha_val, "   λ₁ = ", eigen_val),
           size = 3.5, hjust = 0) +
  labs(
    title    = "Figure 8. Factor Loadings — Perception of Indigenous Peoples Scale",
    subtitle = "One-factor solution (ML); dashed line at 0.3 threshold",
    x        = "Factor loading",
    y        = NULL,
    caption  = paste0("CES 2021. N = ", nrow(fa_items),
                      " (complete cases). Cronbach's α = ", alpha_val,
                      "; first eigenvalue = ", eigen_val, ".")
  ) +
  theme_ajps()

save_fig(fig8, "fig8_factor_loadings", w = 8, h = 4.5)


# =============================================================================
# DONE
# =============================================================================

cat("\n=== ALL FIGURES SAVED TO output/figures/ ===\n")
cat(list.files("output/figures", pattern = "\\.(pdf|png)$"), sep = "\n")
