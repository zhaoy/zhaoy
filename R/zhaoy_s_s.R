#' Summary statistics.
#'
#' @description
#' Compute the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_s(x, s)
#'
#' @param x a numeric or logical vector, or a date / date-time / time-interval object.
#' @param s a summary statistic.
#'
#' @return
#' A length-one character vector.
#'
#' If \code{x} is not a numeric or date / date-time / time-interval object, \code{\link{NA}} is returned.
#'
#' If \code{x} is a date / date-time / time-interval object and the summary statistic is median or mean, \code{\link{NA}} is returned.
#'
#' @importFrom stats median

zhaoy_s_s <- function(x,
                      s) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE),
            length(x = x) >= 1)

  if ((is.numeric(x = x) == FALSE &
       inherits(x = x,
                what = c("Date",
                         "POSIXct",
                         "POSIXlt"),
                which = FALSE) == FALSE) == TRUE |
      (inherits(x = x,
                what = c("Date",
                         "POSIXct",
                         "POSIXlt"),
                which = FALSE) == TRUE &
       s %in% c("median",
                "mean") == TRUE) == TRUE) {

    x_s <- NA

  } else if (is.numeric(x = x) == TRUE) {

    if (s == "min") {

      x_s <- min(x = x,
                 na.rm = TRUE)

    } else if (s == "max") {

      x_s <- max(x = x,
                 na.rm = TRUE)

    } else if (s == "median") {

      x_s <- stats::median(x = x,
                           na.rm = TRUE)

    } else if (s == "mean") {

      x_s <- mean(x = x,
                  trim = 0,
                  na.rm = TRUE)

    }

  } else if (inherits(x = x,
                      what = c("Date",
                               "POSIXct",
                               "POSIXlt"),
                      which = FALSE) == TRUE) {

    if (s == "min") {

      x_s <- min(x = x,
                 na.rm = TRUE)

    } else if (s == "max") {

      x_s <- max(x = x,
                 na.rm = TRUE)

    }

  }

  if (is.na(x = x_s) == TRUE) {

    return(value = NA)

  } else if (is.na(x = x_s) == FALSE) {

    x_s <- as.character(x = x_s)

    return(value = x_s)

  }

}