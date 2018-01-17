#' Summary statistics of numeric data.
#'
#' @description
#' For numeric data, calculate the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_numeric(x, fun)
#'
#' @param x a numeric vector.
#'
#' @return
#' A numeric vector of length one.
#' For non-numeric data, the return value is \code{\link{NA}}.
#'
#' @importFrom stats median
#'
#' @examples
#' zhaoy_s_numeric(x = attenu$accel, fun = "min")

zhaoy_s_numeric <- function(x,
                            fun) {

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