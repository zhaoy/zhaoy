#' Summary statistics of numeric data.
#'
#' @description
#' For numeric data, calculate the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_numeric(x, fun)
#'
#' @param x a vector.
#'
#' @return
#' If \code{x} is numeric, a vector of length one.
#'
#' If \code{x} is non-numeric, a vector of one \code{\link{NA}}.
#'
#' @importFrom dplyr case_when
#' @importFrom stats median
#'
#' @examples
#' zhaoy_s_numeric(x = attenu$accel, fun = "min")

zhaoy_s_numeric <- function(x,
                            fun) {

  stopifnot(is.vector(x = x))

  # Together, case_when conditions should be all-inclusive.
  # Among each other, they should be mutually exclusive.
  # If numeric status and "fun" are both in case_when,
  # x can be non-numeric while "fun" is valid.
  # So it is necessary to determine numeric status first.

  if (is.complex(x = x) == FALSE &
      is.double(x = x) == FALSE &
      is.integer(x = x) == FALSE &
      is.numeric(x = x) == FALSE &
      is.raw(x = x) == FALSE) {

    return(value = NA)

  } else {

    case_when(fun == "min" ~ min(x = x,
                                 na.rm = TRUE),
              fun == "max" ~ max(x = x,
                                 na.rm = TRUE),
              fun == "median" ~ stats::median(x = x,
                                              na.rm = TRUE),
              fun == "mean" ~ mean(x = x,
                                   trim = 0,
                                   na.rm = TRUE))

  }

}