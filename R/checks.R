check_augbin_data            <- function(data) {
  if (!("augbin_data" %in% class(data))) {
    stop("data must be of class augbin_data")
  }
  if (!tibble::is_tibble(data)) {
    stop("data must be a tibble")
  }
  if (ncol(data) != 4) {
    stop("data must have 4 columns")
  }
  desired <- c("outcome", "continuous", "binary", "treatment")
  for (i in c(1, 3, 4)) {
    if (colnames(data)[i] != desired[i]) {
      stop("Column ", i, " of data must be named ", desired[i])
    } else {
      if (!is.factor(data[[desired[i]]])) {
        stop("Column ", desired[i], " in data must be a factor")
      } else {
        if (!all(levels(data[[desired[i]]]) == 0:1)) {
          stop("The levels of column ", desired[i], " must be 0 and 1")
        } else {
          if (any(is.na(data[[desired[i]]]))) {
            stop("Values in column ", desired[i], " must be equal to 0 or 1")
          }
        }
      }
    }
  }
  if (colnames(data)[2] != "continuous") {
    stop("Column 2 of data must be named continuous")
  } else {
    if (!is.numeric(data$continuous)) {
      stop("Column continuous in data must be numeric")
    } else {
      if (any(is.na(data$continuous), is.infinite(data$continuous))) {
        stop("Values in column continuous must be finite")
      }
    }
  }
  if (is.null(attributes(data)$dichotomisation)) {
    stop("data must have attribute dichotomisation")
  } else {
    if (!is.numeric(attributes(data)$dichotomisation)) {
      stop("Attribute dichotomisation in data must be numeric")
    } else if (length(attributes(data)$dichotomisation) > 1) {
      stop("Attribute dichotomisation in data must have length 1")
    } else if (is.infinite(attributes(data)$dichotomisation)) {
      stop("Attribute dichotomisation in data must be finite")
    }
  }
}

check_augbin_data_components <- function(continuous, binary, treatment) {
  if (!is.numeric(continuous)) {
    stop("continuous must be numeric")
  }
  if (!is.numeric(binary)) {
    stop("binary must be numeric")
  }
  if (!is.numeric(treatment)) {
    stop("treatment must be numeric")
  }
  if (length(continuous) != length(binary)) {
    stop("continuous and binary must have the same length")
  }
  if (length(continuous) != length(treatment)) {
    stop("continuous and treatment must have the same length")
  }
  if (any(!is.finite(continuous))) {
    stop("Elements of continuous must be finite")
  }
  if (any(!(binary %in% 0:1))) {
    stop("Elements of binary must be equal to 0 or 1")
  }
  if (any(!(treatment %in% 0:1))) {
    stop("Elements of treatment must be equal to 0 or 1")
  }
}

check_logical                <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_real                   <- function(value, name) {
  if (any(length(value) != 1, !is.numeric(value), is.infinite(value))) {
    stop(name, " must be a single finite numeric")
  }
}