#' Summary statistics.
#'
#' @description
#' Tabulate summary statistics.
#'
#' @usage
#' s_s(x)
#'
#' @param x an R object.
#'
#' @return
#' A data-frame with the following variables:
#'
#' var: variable name.
#'
#' n_miss: count of missing data in variable.
#'
#' pct_miss: percent of variable that has missing data, rounded to one decimal place.
#'
#' n_unique: count of unique values in variable.
#'
#' pct_unique: percent of variable that has unique values, rounded to one decimal place.
#'
#' min and max: If \code{x} is not a numeric or date / date-time / time-interval object, \code{\link{NA}} is returned.
#'
#' median and mean: If \code{x} is a date / date-time / time-interval object, \code{\link{NA}} is returned.
#'
#' mode:
#' If multiple modes exist, "s_mode()" is returned.
#' If the mode is \code{\link{NA}}, \code{\link{NA}} is returned.
#'
#' @seealso \code{\link{s_mode} \link{s_unique}}
#'
#' @importFrom purrr map_chr map_int
#'
#' @export
#'
#' @examples
#' s_s(x = attenu$mag)
#' s_s(x = attenu)

s_s <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "POSIXct",
                              "POSIXlt",
                              "data.frame"),
                     which = FALSE) == TRUE,
            length(x = x) >= 1)

  if (is.data.frame(x = x) == FALSE) {

    x <- data.frame(x = x,
                    row.names = NULL,
                    check.rows = TRUE,
                    check.names = TRUE,
                    fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)

  }

  var <- names(x = x)

  n_miss <- purrr::map_int(.x = x,
                           .f = function(x) sum(is.na(x = x) == TRUE,
                                                na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- purrr::map_int(.x = x,
                             .f = function(x) length(x = unique(x = x,
                                                                incomparables = FALSE)))

  pct_unique <- n_unique / nrow(x = x) * 100

  zhaoy_min <- purrr::map_chr(.x = x,
                              .f = zhaoy_s_s,
                              s = "min")

  zhaoy_max <- purrr::map_chr(.x = x,
                              .f = zhaoy_s_s,
                              s = "max")

  zhaoy_median <- purrr::map_chr(.x = x,
                                 .f = zhaoy_s_s,
                                 s = "median")

  zhaoy_mode <- purrr::map_chr(.x = x,
                               .f = zhaoy_s_mode)

  zhaoy_mean <- purrr::map_chr(.x = x,
                               .f = zhaoy_s_s,
                               s = "mean")

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

  if (nrow(x = x) == 1) {

    subset(x = x,
           select = -var)

  } else if (nrow(x = x) > 1) {

    return(value = x)

  }

}