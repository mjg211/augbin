augbin_dose <- function(min_dose, max_dose, num_doses, log_tumour_ratio,
                        new_lesions, toxicity, dose, baseline_tumour_size,
                        dichot_thresh) {
  model <- augbin_dose_model(log_tumour_ratio, new_lesions, toxicity, dose,
                             baseline_tumour_size)
  doses <- seq(min_dose, max_dose, length.out = num_doses)
  out   <- augbin_dose_ci(doses, model$params, baseline_tumour_size,
                          dichot_thresh, model$cov)
  tibble::tibble(dose = doses, est = ci[, 2], ci_lower = ci[, 1],
                 ci_upper = ci[, 3])
}