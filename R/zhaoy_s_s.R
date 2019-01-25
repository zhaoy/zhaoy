#' Summary Statistics
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
#' A length-one vector.
#'
#' The result is \code{NA} if \code{x} is:
#'
#' missing-data
#'
#' a date / date-time object,
#' and \code{s = "median"} or \code{s = "mean"}
#'
#' not a numeric or date / date-time object.
#'
#' @importFrom dplyr combine
#' @importFrom stats median

zhaoy_s_s <- function(x,
                      s) {

  stopifnot(inherits(x = x,
                     what = dplyr::combine("character",
                                           "integer",
                                           "logical",
                                           "numeric",
                                           "factor",
                                           "Date",
                                           "difftime",
                                           "POSIXct",
                                           "POSIXlt"),
                     which = FALSE) == TRUE,
            is.list(x = x) == FALSE)

  date_time <- inherits(x = x,
                        what = dplyr::combine("Date",
                                              "difftime",
                                              "POSIXct",
                                              "POSIXlt"),
                        which = FALSE)

  if (is.na(x = x) == TRUE ||
      (is.numeric(x = x) == FALSE &&
       date_time == FALSE) == TRUE ||
      (date_time == TRUE &&
       s %in% dplyr::combine("median",
                             "mean") == TRUE) == TRUE) {

    s_s <- NA

  } else if (is.numeric(x = x) == TRUE) {

    s_s <- switch(EXPR = s,
                  min = min(x = x,
                            na.rm = TRUE),
                  max = max(x = x,
                            na.rm = TRUE),
                  median = stats::median(x = x,
                                         na.rm = TRUE),
                  mean = mean(x = x,
                              trim = 0,
                              na.rm = TRUE))

  } else if (date_time == TRUE) {

    s_s <- switch(EXPR = s,
                  min = min(x = x,
                            na.rm = TRUE),
                  max = max(x = x,
                            na.rm = TRUE))

  }

  s_s

}