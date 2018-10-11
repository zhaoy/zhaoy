#' Summary statistics.
#'
#' @description
#' Compute the minimum, maximum, median, or mean.
#'
#' @usage
#' zhaoy_s_s(x, s)
#'
#' @param x an R object.
#' @param s a statistic.
#'
#' @return
#' A length-one character vector.
#'
#' If \code{x} is not a numeric or date / date-time / time-interval object,
#' and the statistic is "min" or "max",
#' \code{\link{NA}} is returned.
#'
#' If \code{x} is a date / date-time / time-interval object,
#' and the statistic is median or mean,
#' \code{\link{NA}} is returned.
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
                              "difftime",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE),
            length(x = x) >= 1)

  date_time_interval <- inherits(x = x,
                                 what = c("Date",
                                          "difftime",
                                          "POSIXct",
                                          "POSIXlt"),
                                 which = FALSE)

  if ((is.numeric(x = x) == FALSE &
       date_time_interval == FALSE) == TRUE |
      (date_time_interval == TRUE &
       s %in% c("median",
                "mean") == TRUE) == TRUE) {

    x_s <- NA_character_

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

      x_s <- round(x = x_s,
                   digits = 1)

    }

  } else if (date_time_interval == TRUE) {

    if (s == "min") {

      x_s <- min(x = x,
                 na.rm = TRUE)

    } else if (s == "max") {

      x_s <- max(x = x,
                 na.rm = TRUE)

    }

  }

  as.character(x = x_s)

}