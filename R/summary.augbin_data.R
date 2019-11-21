#' Print a summary of an augbin_data object
#'
#' @param object A ...
#'
#' @export
summary.augbin_data <- function(object, ...) {

  ##### Check input variables ##################################################

  check_augbin_data(object)

  ##### Print a summary ########################################################

  summary_augbin_data(object)

}

#' @rdname summary.augbin_data
print.augbin_data <- function(object, ...) {

  ##### Check input variables ##################################################

  check_augbin_data(object)

  ##### Print a summary ########################################################

  summary_augbin_data(object)

}