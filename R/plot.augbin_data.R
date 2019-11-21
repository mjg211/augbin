#' @export
plot.augbin_data <- function(x       = build_augbin_data(),
                             output  = FALSE,
                             summary = FALSE,
                             ...) {

  ##### Check input variables ##################################################

  check_augbin_data(x)
  check_logical(output, "output")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  x_internal            <- dplyr::arrange(x, treatment, continuous)
  data_summary          <- dplyr::summarise(dplyr::group_by(x_internal,
                                                            treatment),
                                            n = dplyr::n())
  x_internal            <- dplyr::mutate(x_internal,
                                         patient = c(1:data_summary$n[1],
                                                     1:data_summary$n[2]))
  x_internal$treatment2 <-
    factor(x_internal$treatment,
           labels = c(paste("Treatment~0~(italic(n)[0]==", data_summary$n[1],
                            ")", sep = ""),
                      paste("Treatment~1~(italic(n)[1]==", data_summary$n[2],
                            ")", sep = "")))
  plot                  <-
    ggplot2::ggplot(data = x_internal,
                    ggplot2::aes(x      = patient,
                                 y      = continuous,
                                 shape  = binary,
                                 colour = outcome)) +
    ggplot2::geom_point() +
    ggthemes::scale_colour_ptol() +
    ggplot2::facet_grid(. ~ treatment2,
                        labeller = label_parsed) +
    ggplot2::geom_hline(yintercept = attributes(x)$dichotomisation,
                        linetype   = 2,
                        colour     = "darkgray") +
    ggplot2::scale_x_continuous(name   = "Patient",
                                breaks = NULL) +
    ggplot2::ylab("Continuous component") +
    ggplot2::labs(shape  = "Binary component",
                  colour = "Responder outcome") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  print(plot)

  ##### Outputting #############################################################

  if (output) {
    return(list(plot   = plot,
                inputs = list(output  = output,
                              summary = summary,
                              x       = x)))
  }

}