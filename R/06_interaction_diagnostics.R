library(dplyr)
library(readr)

setwd("/home/ral/.openclaw/workspace/urban_proximity")

dir.create("output", showWarnings = FALSE)

riding_means <- readRDS("data-clean/riding_means.rds")
prox_buffers <- read_csv("data-clean/proximity_fed_enhanced.csv", show_col_types = FALSE)

riding_means <- riding_means |>
  left_join(
    prox_buffers |>
      transmute(feduid = as.numeric(FEDUID), log_reserves_50km),
    by = "feduid"
  )

extract_term_row <- function(coef_mat, term) {
  if (term %in% rownames(coef_mat)) {
    return(coef_mat[term, , drop = FALSE])
  }

  if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    alt <- paste(rev(parts), collapse = ":")
    if (alt %in% rownames(coef_mat)) {
      return(coef_mat[alt, , drop = FALSE])
    }
  }

  NULL
}

format_term <- function(coef_mat, term) {
  row <- extract_term_row(coef_mat, term)
  if (is.null(row)) {
    return(data.frame(term = term, estimate = NA_real_, se = NA_real_, p_value = NA_real_))
  }

  data.frame(
    term = term,
    estimate = unname(row[1, 1]),
    se = unname(row[1, 2]),
    p_value = unname(row[1, ncol(row)]),
    row.names = NULL
  )
}

run_spec <- function(spec_name, data, formula, key_terms) {
  model <- lm(formula, data = data)
  coef_mat <- summary(model)$coefficients
  key_df <- do.call(rbind, lapply(key_terms, function(term) format_term(coef_mat, term)))

  cat(sprintf("\n%s\n", spec_name))
  cat(sprintf("N=%d | R2=%.4f\n", nobs(model), summary(model)$r.squared))
  print(key_df)

  data.frame(
    spec = spec_name,
    N = nobs(model),
    R2 = summary(model)$r.squared,
    term = key_df$term,
    estimate = key_df$estimate,
    se = key_df$se,
    p_value = key_df$p_value,
    row.names = NULL
  )
}

base_controls <- c("log_area_nonres_km2", "mean_ideology", "mean_educ", "mean_age", "pct_male", "pct_born_canada")

spec_a_data <- riding_means |>
  mutate(cwb_centered = nearest_reserve_cwb - mean(nearest_reserve_cwb, na.rm = TRUE)) |>
  filter(!is.na(mean_therm), !is.na(log_dist_nearest_km), !is.na(cwb_centered), !is.na(log_area_nonres_km2),
         !is.na(mean_ideology), !is.na(mean_educ), !is.na(mean_age), !is.na(pct_male), !is.na(pct_born_canada))

spec_b_data <- riding_means |>
  mutate(cwb_centered = nearest_reserve_cwb - mean(nearest_reserve_cwb, na.rm = TRUE)) |>
  filter(!is.na(mean_therm), !is.na(log_reserves_50km), !is.na(cwb_centered), !is.na(log_area_nonres_km2),
         !is.na(mean_ideology), !is.na(mean_educ), !is.na(mean_age), !is.na(pct_male), !is.na(pct_born_canada))

spec_c_data <- riding_means |>
  mutate(
    cwb_quartile = cut(
      nearest_reserve_cwb,
      breaks = quantile(nearest_reserve_cwb, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE
    )
  ) |>
  filter(!is.na(mean_therm), !is.na(log_dist_nearest_km), !is.na(cwb_quartile), !is.na(log_area_nonres_km2),
         !is.na(mean_ideology), !is.na(mean_educ), !is.na(mean_age), !is.na(pct_male), !is.na(pct_born_canada))

spec_d_data <- riding_means |>
  mutate(cwb_centered = nearest_reserve_cwb - mean(nearest_reserve_cwb, na.rm = TRUE)) |>
  filter(!is.na(mean_therm), !is.na(log_dist_nearest_km), !is.na(cwb_centered), !is.na(log_area_nonres_km2),
         !is.na(mean_ideology), !is.na(mean_educ), !is.na(mean_age), !is.na(pct_male), !is.na(pct_born_canada), !is.na(n_respondents))

results <- bind_rows(
  run_spec(
    "a_centered_cwb_x_distance",
    spec_a_data,
    as.formula(paste("mean_therm ~ log_dist_nearest_km * cwb_centered +", paste(base_controls, collapse = " + "))),
    c("log_dist_nearest_km", "cwb_centered", "log_dist_nearest_km:cwb_centered")
  ),
  run_spec(
    "b_log_reserves_50km_x_centered_cwb",
    spec_b_data,
    as.formula(paste("mean_therm ~ log_reserves_50km * cwb_centered +", paste(base_controls, collapse = " + "))),
    c("log_reserves_50km", "cwb_centered", "log_reserves_50km:cwb_centered")
  ),
  run_spec(
    "c_cwb_quartiles_x_distance",
    spec_c_data,
    as.formula(paste("mean_therm ~ log_dist_nearest_km * cwb_quartile +", paste(base_controls, collapse = " + "))),
    c("log_dist_nearest_km", "cwb_quartileQ2", "cwb_quartileQ3", "cwb_quartileQ4",
      "log_dist_nearest_km:cwb_quartileQ2", "log_dist_nearest_km:cwb_quartileQ3", "log_dist_nearest_km:cwb_quartileQ4")
  ),
  run_spec(
    "d_riding_level_centered_cwb_n338",
    spec_d_data,
    as.formula(paste("mean_therm ~ log_dist_nearest_km * cwb_centered + n_respondents +", paste(base_controls, collapse = " + "))),
    c("log_dist_nearest_km", "cwb_centered", "log_dist_nearest_km:cwb_centered", "n_respondents")
  )
)

write_csv(results, "output/interaction_diagnostics.csv")
cat("\nSaved output/interaction_diagnostics.csv\n")
