#' Summary statistics.
#'
#' @description
#' Tabulate summary statistics of data-frames, vectors, factors, and dates / date-times.
#'
#' @usage
#' s_s(x)
#'
#' @param x a data-frame, vector, factor, or one or more dates / date-times.
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' var: variable names, included only if \code{x} is a data-frame.
#'
#' n_miss: numbers of missing data.
#'
#' pct_miss: missing data as percents rounded to the nearest integers.
#'
#' n_unique: numbers of unique values.
#'
#' pct_unique: unique values as percents rounded to the nearest integers.
#'
#' min, max, median, mean: if the variable is non-numeric, \code{\link{NA}} is returned.
#'
#' mode: if \code{\link{NA}} is the most frequent value in the variable, \code{\link{NA}} or "<NA>" is returned.
#' If the variable has multiple modes, "> 1 mode" is returned.
#' If the variable has a length of one, or has otherwise no mode, "no mode" is returned.
#'
#' @seealso \code{\link{s_cp} \link{s_mode}}
#'
#' @importFrom dplyr n_distinct select
#' @importFrom purrr map map_dbl map_int modify_at
#'
#' @export
#'
#' @examples
#' s_s(x = attenu$mag)
#' s_s(x = attenu)

s_s <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
            is.data.frame(x = x) |
            is.factor(x = x) |
            is.vector(x = x))

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

  n_miss <- purrr::map_int(.x = x,
                           .f = function(x) sum(is.na(x = x) == TRUE,
                                                na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- purrr::map_int(.x = x,
                             .f = dplyr::n_distinct,
                             na.rm = TRUE)

  pct_unique <- n_unique / nrow(x = x) * 100

  zhaoy_min <- purrr::map_dbl(.x = x,
                              .f = zhaoy_s_numeric,
                              fun = "min")

  zhaoy_max <- purrr::map_dbl(.x = x,
                              .f = zhaoy_s_numeric,
                              fun = "max")

  zhaoy_median <- purrr::map_dbl(.x = x,
                                 .f = zhaoy_s_numeric,
                                 fun = "median")

  zhaoy_mode <- unlist(x = purrr::map(.x = x,
                                      .f = zhaoy_mode))

  zhaoy_mean <- purrr::map_dbl(.x = x,
                               .f = zhaoy_s_numeric,
                               fun = "mean")

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

  x <- purrr::modify_at(.x = x,
                        .at = c("pct_miss",
                                "pct_unique"),
                        .f = round,
                        digits = 0)

  # To retain their numeric status,
  # do NOT drop trailing zeros
  # from numbers in the min, max, median, and mean columns.

  if (nrow(x = x) == 1) {

    dplyr::select(.data = x,
                  n_miss:mean)

  } else if (nrow(x = x) > 1) {

    return(value = x)

  }

}