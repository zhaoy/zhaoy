#' Summary statistics of numeric data
#'
#' @description
#' If data are numeric, calculate the minimum, maximum, median, and mean.
#'
#' @usage
#' zhaoy_numeric(x, fun)
#'
#' @param x a vector.
#'
#' @return A vector.
#'
#' @importFrom stats median

zhaoy_numeric <- function(x,
                          fun) {

  if (is.numeric(x = x) == TRUE) {

    if (fun == "min") {

      min(x = x,
          na.rm = TRUE)

    }

    else if (fun == "max") {

      max(x = x,
          na.rm = TRUE)

    }

    else if (fun == "median") {

      stats::median(x = x,
                    na.rm = TRUE)

    }

    else if (fun == "mean") {

      mean(x = x,
           trim = 0,
           na.rm = TRUE)

    }

  } else if (is.numeric(x = x) == FALSE) {

    return(value = NA)

  }

}