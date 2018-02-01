#' Summary statistics.
#'
#' @description
#' For numeric data, calculate the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_numeric(x, s)
#'
#' @param x a vector, factor, or one or more dates / date-times.
#'
#' @return
#' A vector.
#'
#' If \code{x} is numeric, a number is returned.
#'
#' If \code{x} is non-numeric, \code{\link{NA}} is returned.
#'
#' @importFrom stats median
#'
#' @examples
#' zhaoy_s_numeric(x = attenu$accel, s = "min")

zhaoy_s_numeric <- function(x,
                            s) {

  stopifnot((inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
             is.factor(x = x) |
             is.vector(x = x)),
            length(x = x) >= 1)

  # Conceptually and functionally,
  # min(), max(), and median() are compatible with dates / date-times.
  # But because dates / date-times and numbers cannot co-exist in vectors,
  # s_s() and therefore zhaoy_s_numeric() execute min(), max(), and median()
  # only for numeric data.

  if (is.numeric(x = x) == FALSE) {

    return(value = NA)

  } else if (is.numeric(x = x) == TRUE) {

    if (s == "min") {

      min(x = x,
          na.rm = TRUE)

    } else if (s == "max") {

      max(x = x,
          na.rm = TRUE)

    } else if (s == "median") {

      stats::median(x = x,
                    na.rm = TRUE)

    } else if (s == "mean") {

      mean(x = x,
           trim = 0,
           na.rm = TRUE)

    }

  }

}