#' Summary statistics of numeric data.
#'
#' @description
#' For numeric data, calculate the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_numeric(x, fun)
#'
#' @param x a vector or factor.
#'
#' @return
#' If \code{x} is numeric, a vector of one statistic.
#'
#' If \code{x} is non-numeric, a vector of one \code{\link{NA}}.
#'
#' @importFrom stats median
#'
#' @examples
#' zhaoy_s_numeric(x = attenu$accel, fun = "min")

zhaoy_s_numeric <- function(x,
                            fun) {

  stopifnot(is.factor(x = x) |
            is.vector(x = x))

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