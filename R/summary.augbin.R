#' Print a summary of an augbin object
#'
#' @param object A ...
#'
#' @export
summary.augbin <- function(object, ...) {

  ##### Check input variables ##################################################

  check_augbin(object)

  ##### Print a summary ########################################################

  summary_augbin(object)

}

#' @rdname print.augbin_data
print.augbin <- function(object, ...) {

  ##### Check input variables ##################################################

  check_augbin(object)

  ##### Print a summary ########################################################

  summary_augbin(object)

}