#' Build a dataset of class \code{"augbin_data"}
#'
#' Builds a dataset of class \code{"augbin_data"}, principally for use with
#' \code{\link[augbin]{augbin}}.
#'
#' The responder outcome is assumed to define a responder when
#' a continuous component (see \code{continuous}) is below a particular
#' dichotomisation threshold (see \code{dichotomisation}) and a binary component
#' (see \code{binary}) is 0.
#'
#' @param continuous A \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' containing the observations of a continuous component of a responder outcome.
#' Each element should correspond to the observation for a different patient.
#' @param binary A \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' containing the observations of a binary failure component of a responder
#' outcome. Each element of \code{binary} must be either 0 or 1 and its
#' \code{\link[base]{length}} should be equal to that of \code{continuous}. In
#' addition, its elements should be ordered such that the observation for a
#' particular patient is in the same position as it is in \code{continuous}.
#' @param treatment A \code{\link[base]{numeric}} \code{\link[base]{vector}}
#' containing the treatment assignments. Each element of \code{treatment} must
#' be either 0 (corresponding to the 'control' arm) or 1 (corresponding to the
#' 'experimental' arm). In addition, its elements should be ordered such that
#' the treatment assignment for a particular patient is in the same position as
#' it is in \code{continuous}.
#' @param dichotomisation A \code{\link[base]{numeric}} giving the
#' dichotomisation hreshold for the continuous component (see
#' \code{continuous}). A patient is a responder if their binary component (see
#' \code{binary}) is 0 and their continuous component is below
#' \code{dichotomisation}.
#' @return A \code{\link[tibble]{tibble}}, with additional
#' \code{\link[base]{class}} \code{"augbin_data"}.
#' @author Michael J Grayling.
#' @seealso \code{\link[augbin]{augbin}}, \code{\link[augbin]{print.augbin}},
#' \code{\link[augbin]{summary.augbin}}, \code{\link[augbin]{plot.augbin}}.
#' @examples
#' # Create a randomly generated dataset from 100 patients
#' data <- build_augbin_data()
#' @export
build_augbin_data <- function(continuous      = stats::rnorm(100),
                              binary          = stats::rbinom(100, 1, 0.4),
                              treatment       = stats::rbinom(100, 1, 0.5),
                              dichotomisation = 0,
                              summary         = FALSE) {

  ##### Check input parameters #################################################

  check_augbin_data_components(continuous, binary, treatment)
  check_real(dichotomisation, "dichotomisation")
  check_logical(summary, "summary")

  ##### Print summary ##########################################################

  if (summary) {
    message("  -------------------------------------------------------")
    message("  build_augbin_data: Building an augmented-binary dataset")
    message("  -------------------------------------------------------")
    message("")
    message("  -----------")
    message("  Calculating")
    message("  -----------")
    message("  Beginning required calculations\u2026\u2026")
  }

  ##### Main computations ######################################################

  data                          <-
    tibble::tibble(outcome    =
                     factor(as.numeric(binary == 0 &
                                         continuous < dichotomisation), 0:1),
                   continuous = continuous,
                   binary     = factor(binary, 0:1),
                   treatment  = factor(treatment, 0:1))
  attr(data, "dichotomisation") <- dichotomisation
  class(data)                   <- c("augbin_data", class(data))

  if (summary) {
    message("\u2026\u2026completed required calculations.")
    message("")
    summary_augbin_data(data)
  }

  ##### Outputting #############################################################

  data

}