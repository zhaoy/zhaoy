#' Summary statistics.
#'
#' @description
#' For numeric data, calculate the minimum, maximum, median, or mean.
#'
#' For dates / date-times, calculate the minimum and maximum.
#'
#' @usage
#' zhaoy_s_numeric(x, fun)
#'
#' @param x a vector, factor, or one or more dates / date-times.
#'
#' @return
#' If \code{x} is numeric, a length-one numeric vector.
#'
#' If \code{x} is non-numeric, a vector of one \code{\link{NA}}.
#'
#' @importFrom stats median
#'
#' @examples
#' zhaoy_s_numeric(x = attenu$accel, fun = "min")

zhaoy_s_numeric <- function(x,
                            fun) {

  stopifnot(inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
            is.factor(x = x) |
            is.vector(x = x))

  # Conceptually and functionally,
  # min(), max(), and median() are compatible with dates / date-times.
  # But because dates / date-times and numbers cannot co-exist in vectors,
  # s_s() and therefore zhaoy_s_numeric() execute min(), max(), and median()
  # only for numeric data.

  if (is.numeric(x = x) == FALSE) {

    return(value = NA)

  } else if (is.numeric(x = x) == TRUE) {

    if (fun == "min") {

      min(x = x,
          na.rm = TRUE)

    } else if (fun == "max") {

      max(x = x,
          na.rm = TRUE)

    } else if (fun == "median") {

      stats::median(x = x,
                    na.rm = TRUE)

    } else if (fun == "mean") {

      mean(x = x,
           trim = 0,
           na.rm = TRUE)

    }

  }

}