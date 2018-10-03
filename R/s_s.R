#' Summary statistics.
#'
#' @description
#' Tabulate summary statistics of data-frames, vectors, factors, and POSIXlt / POSIXct objects.
#'
#' @usage
#' s_s(x)
#'
#' @param x a data-frame, vector, factor, or one or more POSIXlt / POSIXct objects.
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' var: variable names, included only if \code{x} is a data-frame.
#'
#' n_miss: count of missing data.
#'
#' pct_miss: missing data as percents of variables, rounded to one decimal place.
#'
#' n_unique: count of unique values.
#'
#' pct_unique: unique values as percents of variables, rounded to one decimal place.
#'
#' min, max, median, mean: if a variable is non-numeric, \code{\link{NA}} is returned.
#'
#' mode: if \code{\link{NA}} is the most frequent element in a variable, \code{\link{NA}} or "<NA>" is returned.
#' If a variable has one element, that element is returned.
#' If a variable has multiple modes, "s_mode()" is returned.
#' If a variable has multiple elements and no mode, "no mode" is returned.
#'
#' @seealso \code{\link{s_mode} \link{s_unique}}
#'
#' @export
#'
#' @examples
#' s_s(x = attenu$mag)
#' s_s(x = attenu)

s_s <- function(x) {

  stopifnot((inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
             is.data.frame(x = x) |
             is.factor(x = x) |
             is.vector(x = x)),
            (length(x = x) >= 1 |
             (nrow(x = x) >= 1 &
              ncol(x = x) >= 1)))

  if (inherits(x = x,
               what = c("Date",
                        "POSIXct",
                        "POSIXlt"),
               which = FALSE) == TRUE |
      is.factor(x = x) == TRUE |
      is.vector(x = x) == TRUE) {

    x <- as.data.frame(x = x,
                       row.names = NULL,
                       stringsAsFactors = FALSE,
                       cut.names = TRUE,
                       fix.empty.names = TRUE)

  }

  var <- names(x = x)

  n_miss <- vapply(X = x,
                   FUN = function(x) sum(is.na(x = x) == TRUE,
                                         na.rm = FALSE),
                   FUN.VALUE = 1,
                   USE.NAMES = FALSE)

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- vapply(X = x,
                     FUN = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)),
                     FUN.VALUE = 1,
                     USE.NAMES = FALSE)

  pct_unique <- n_unique / nrow(x = x) * 100

  zhaoy_min <- vapply(X = x,
                      FUN = zhaoy_s_s,
                      s = "min",
                      FUN.VALUE = 1,
                      USE.NAMES = FALSE)

  zhaoy_max <- vapply(X = x,
                      FUN = zhaoy_s_s,
                      s = "max",
                      FUN.VALUE = 1,
                      USE.NAMES = FALSE)

  zhaoy_median <- vapply(X = x,
                         FUN = zhaoy_s_s,
                         s = "median",
                         FUN.VALUE = 1,
                         USE.NAMES = FALSE)

  zhaoy_mode <- lapply(X = x,
                       FUN = zhaoy_s_mode)

  zhaoy_mode <- unlist(x = zhaoy_mode)

  zhaoy_mean <- vapply(X = x,
                       FUN = zhaoy_s_s,
                       s = "mean",
                       FUN.VALUE = 1,
                       USE.NAMES = FALSE)

  x <- data.frame(var,
                  n_miss,
                  pct_miss,
                  n_unique,
                  pct_unique,
                  min = zhaoy_min,
                  max = zhaoy_max,
                  median = zhaoy_median,
                  mode = zhaoy_mode,
                  mean = zhaoy_mean,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  x[, c("pct_miss",
        "pct_unique")] <- round(x = x[, c("pct_miss",
                                          "pct_unique")],
                                digits = 1)

  # To retain their numeric status,
  # do NOT drop trailing zeros
  # from numbers in the min, max, median, and mean columns.

  if (nrow(x = x) == 1) {

    subset(x = x,
           select = -var)

  } else if (nrow(x = x) > 1) {

    return(value = x)

  }

}