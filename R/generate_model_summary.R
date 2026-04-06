library(readr)

setwd("/home/ral/.openclaw/workspace/urban_proximity")

dir.create("output", showWarnings = FALSE)

models <- readRDS("data-clean/models_revised.rds")
riding_means <- readRDS("data-clean/riding_means.rds")

get_term_row <- function(coef_mat, term) {
  if (term %in% rownames(coef_mat)) {
    return(coef_mat[term, , drop = FALSE])
  }

  alt_terms <- c(
    if (term == "log_dist_nearest_km:nearest_reserve_cwb") "nearest_reserve_cwb:log_dist_nearest_km",
    if (term == "nearest_reserve_cwb:log_dist_nearest_km") "log_dist_nearest_km:nearest_reserve_cwb"
  )
  alt_terms <- alt_terms[!is.na(alt_terms)]

  for (alt in alt_terms) {
    if (alt %in% rownames(coef_mat)) {
      return(coef_mat[alt, , drop = FALSE])
    }
  }

  NULL
}

extract_term <- function(model, term) {
  coef_mat <- summary(model)$coefficients
  row <- get_term_row(coef_mat, term)

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

extract_model_summary <- function(model_name, model) {
  terms_to_pull <- c(
    "log_dist_nearest_km",
    "nearest_reserve_cwb",
    "log_dist_nearest_km:nearest_reserve_cwb"
  )

  term_rows <- do.call(rbind, lapply(terms_to_pull, function(term) extract_term(model, term)))
  term_rows <- as.data.frame(term_rows)

  out <- data.frame(
    model = model_name,
    N = nobs(model),
    R2 = summary(model)$r.squared,
    log_dist_nearest_km_coef = term_rows$estimate[term_rows$term == "log_dist_nearest_km"],
    log_dist_nearest_km_se = term_rows$se[term_rows$term == "log_dist_nearest_km"],
    log_dist_nearest_km_p = term_rows$p_value[term_rows$term == "log_dist_nearest_km"],
    nearest_reserve_cwb_coef = term_rows$estimate[term_rows$term == "nearest_reserve_cwb"],
    nearest_reserve_cwb_se = term_rows$se[term_rows$term == "nearest_reserve_cwb"],
    nearest_reserve_cwb_p = term_rows$p_value[term_rows$term == "nearest_reserve_cwb"],
    interaction_coef = term_rows$estimate[term_rows$term == "log_dist_nearest_km:nearest_reserve_cwb"],
    interaction_se = term_rows$se[term_rows$term == "log_dist_nearest_km:nearest_reserve_cwb"],
    interaction_p = term_rows$p_value[term_rows$term == "log_dist_nearest_km:nearest_reserve_cwb"],
    row.names = NULL
  )

  out
}

model_names <- c("mA", "mB", "mC", "mD")
summary_df <- do.call(rbind, lapply(model_names, function(name) extract_model_summary(name, models[[name]])))

write_csv(summary_df, "output/model_summary_revised.csv")

txt_lines <- c(
  "Revised model summary",
  sprintf("Riding means rows available: %d", nrow(riding_means)),
  ""
)

for (i in seq_len(nrow(summary_df))) {
  row <- summary_df[i, ]
  txt_lines <- c(
    txt_lines,
    sprintf("%s: N=%d | R2=%.4f", row$model, row$N, row$R2),
    sprintf("  log_dist_nearest_km: coef=%.6f se=%.6f p=%.6g",
            row$log_dist_nearest_km_coef, row$log_dist_nearest_km_se, row$log_dist_nearest_km_p),
    sprintf("  nearest_reserve_cwb: coef=%s se=%s p=%s",
            ifelse(is.na(row$nearest_reserve_cwb_coef), "NA", sprintf("%.6f", row$nearest_reserve_cwb_coef)),
            ifelse(is.na(row$nearest_reserve_cwb_se), "NA", sprintf("%.6f", row$nearest_reserve_cwb_se)),
            ifelse(is.na(row$nearest_reserve_cwb_p), "NA", format(row$nearest_reserve_cwb_p, digits = 6))),
    sprintf("  interaction: coef=%s se=%s p=%s",
            ifelse(is.na(row$interaction_coef), "NA", sprintf("%.6f", row$interaction_coef)),
            ifelse(is.na(row$interaction_se), "NA", sprintf("%.6f", row$interaction_se)),
            ifelse(is.na(row$interaction_p), "NA", format(row$interaction_p, digits = 6))),
    ""
  )
}

writeLines(txt_lines, "output/model_summary_revised.txt")

cat("Saved output/model_summary_revised.csv\n")
cat("Saved output/model_summary_revised.txt\n")
