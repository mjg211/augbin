#' Augmented binary method
#'
#' Fits the augmented binary method, using an underlying two-component latent
#' variable model.
#'
#' The responder outcome is assumed to define a responder when
#' a continuous component is below a particular dichotomisation threshold and
#' a binary component is 0.
#'
#' @param data A dataset to analyse. Must be an object of class
#' \code{"augbin_data"}. See \code{\link[augbin]{build_augbin_data}} for further
#' details. Defaults to \code{build_augbin_data()}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether to
#' print a summary of the functions progress. Defaults to \code{FALSE}.
#' @return A \code{\link[base]{list}} of additional \code{\link[base]{class}}
#' \code{"augbin"}, to enable use with several available S3 functions. It will
#' contain two components:
#' \itemize{
#' \item A \code{\link[tibble]{tibble}} in the slot \code{"odds_ratio"} with
#' three columns, giving the estimated odds ratio and the limits of its 95\%
#' confidence interval.
#' \item A \code{\link[base]{list}} in the slot \code{"inputs"}, containing each
#' of the input variable.
#' }
#' @author James MS Wason, Michael J Grayling.
#' @seealso \code{\link[augbin]{build_augbin_data}},
#' \code{\link[augbin]{print.augbin}}, \code{\link[augbin]{summary.augbin}},
#' \code{\link[augbin]{plot.augbin}}.
#' @examples
#' # Use the augmented binary method on a randomly generated dataset
#' odds_ratio <- augbin()
#' @export
augbin <- function(data    = build_augbin_data(),
                   summary = FALSE) {

  ##### Check input parameters #################################################

  check_augbin_data(data)
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    message("  -------------------------------")
    message("  augbin: Augmented binary method")
    message("  -------------------------------")
    message("")
    summary_augbin_data(data)
  }

  ##### Main computations ######################################################

  if (summary) {
    message("")
    message("  -----------")
    message("  Calculating")
    message("  -----------")
    message("  Beginning required calculations\u2026\u2026")
  }
  model <- augbin_model(data$continuous,
                        as.numeric(as.character(data$binary)),
                        as.numeric(as.character(data$treatment)))
  out   <- augbin_ci(model$params, data$continuous,
                     attributes(data)$dichotomisation, model$cov)
  if (summary) {
    message("\u2026\u2026completed required calculations.")
    message("")
    summary_augbin(output)
  }

  ##### Outputting #############################################################

  output        <- list(odds_ratio = tibble::tibble(est      = out[2],
                                                    ci_lower = out[1],
                                                    ci_upper = out[3]),
                        inputs     = list(data    = data,
                                          summary = summary))
  class(output) <- c(class(output), "augbin")
  output

}