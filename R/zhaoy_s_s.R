#' Summary statistics.
#'
#' @description
#' Calculate the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_s(x, s)
#'
#' @param x a numeric or logical vector, or a date / date-time / time-interval object.
#' @param s a summary statistic.
#'
#' @return
#' A length-one vector.
#'
#' If \code{x} is non-numeric and not a date / date-time / time-interval object, \code{\link{NA}} is returned.
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
                which = FALSE) == FALSE) |
      (inherits(x = x,
                what = c("Date",
                         "POSIXct",
                         "POSIXlt"),
                which = FALSE) == TRUE &
       s %in% c("median",
                "mean") == TRUE)) {

    return(value = NA)

  } else if (is.numeric(x = x) == TRUE |
             inherits(x = x,
                      what = c("Date",
                               "POSIXct",
                               "POSIXlt"),
                      which = FALSE) == TRUE) {

    if (s == "min") {

      min(x = x,
          na.rm = TRUE)

    } else if (s == "max") {

      max(x = x,
          na.rm = TRUE)

    }

  } else if (is.numeric(x = x) == TRUE &
             inherits(x = x,
                      what = c("Date",
                               "POSIXct",
                               "POSIXlt"),
                      which = FALSE) == FALSE) {

    if (s == "median") {

      stats::median(x = x,
                    na.rm = TRUE)

    } else if (s == "mean") {

      mean(x = x,
           trim = 0,
           na.rm = TRUE)

    }

  }

}