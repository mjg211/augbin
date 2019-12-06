summary_augbin <- function(object) {
  message("  ----------------------------------")
  message("  Results: Augmented binary analysis")
  message("  ----------------------------------")
  message("  Estimated odds ratio: ", signif(object$odds_ratio$est, 3),
          " [95% CI: (", signif(object$odds_ratio$ci_lower, 3), ", ",
          signif(object$odds_ratio$ci_upper, 3), ")]")
}

summary_augbin_data <- function(data) {
  message("  ------------------------------")
  message("  Summary: Augmented binary data")
  message("  ------------------------------")
  data_summary <- dplyr::summarise(dplyr::group_by(data, treatment),
                                   n         = dplyr::n(),
                                   mean      = signif(mean(continuous), 3),
                                   sd        = signif(sqrt(var(continuous)), 3),
                                   bin       = sum(binary == 0),
                                   perc_bin  = round(100*bin/n, 1),
                                   resp      = sum(outcome == 1),
                                   perc_resp = round(100*resp/n, 1))
  message("  A dataset of observations from:")
  message("    \u2022 ", data_summary$n[1], " patients on treatment 0;")
  message("    \u2022 ", data_summary$n[2], " patients on treatment 1.")
  message("")
  message("  On treatment 0:")
  message("    \u2022 the mean (sd) of the continuous component is ",
          data_summary$mean[1], " (", data_summary$sd[1], ");")
  message("    \u2022 ", data_summary$bin[1], "/", data_summary$n[1],
          " patients (", data_summary$perc_bin[1], "%) patients have a binary ",
          "component of 0.")
  message("")
  message("  On treatment 1:")
  message("    \u2022 the mean (sd) of the continuous component is ",
          data_summary$mean[2], " (", data_summary$sd[2], ");")
  message("    \u2022 ", data_summary$bin[2], "/", data_summary$n[2],
          " patients (", data_summary$perc_bin[2], "%) patients have a binary ",
          "component of 0.")
  message("")
  message("  Patients are considered to be a responder when:")
  message("    \u2022 their binary component is 0 and;")
  message("    \u2022 their continuous component is below ",
          attributes(data)$dichotomisation, ".")
  message("")
  message("  Using this definition:")
  message("    \u2022 ", data_summary$resp[1], "/", data_summary$n[1],
          " patients (", data_summary$perc_resp[1], "%) on treatment 0 are ",
          "responders;")
  message("    \u2022 ", data_summary$resp[2], "/", data_summary$n[2],
          " patients (", data_summary$perc_resp[2], "%) on treatment 1 are ",
          "responders.")
}