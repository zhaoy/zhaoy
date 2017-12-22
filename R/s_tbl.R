#' Summary statistics of columns and tables
#'
#' @description
#' Tabulate summary statistics of columns and tables.
#'
#' @usage
#' s_tbl(x)
#'
#' @param x a column or table
#'
#' @return A table.
#'
#' @details
#' The outputs are:
#'
#' col: column name, displayed only when input is a table
#'
#' n_miss: number of missing data
#'
#' pct_miss: percent of column that are missing data
#'
#' n_unique: number of unique values in column
#'
#' pct_unique: percent of column that are unique values
#'
#' min, max, median, mean: \code{\link{NA}} when a column is non-numeric
#'
#' mode: \code{\link{NA}} only when \code{\link{NA}} is the most frequent value in a column.
#' If there is no mode, the displayed value is "no mode".
#' If there is more than one mode, the displayed value is "> 1 mode".
#'
#' @seealso \code{\link{s_col} \link{s_mode}}
#'
#' @import purrr
#' @import dplyr
#'
#' @export
#'
#' @examples
#' s_tbl(x = attenu$station)
#' s_tbl(x = attenu)

s_tbl <- function(x) {

  stopifnot(is.data.frame(x = x) |
            is.factor(x = x) |
            is.vector(x = x))

  if (is.factor(x = x) == TRUE |
      is.vector(x = x) == TRUE) {

    x <- as.data.frame(x = x,
                       row.names = NULL,
                       stringsAsFactors = FALSE,
                       cut.names = TRUE,
                       fix.empty.names = TRUE)

  }

  col <- names(x = x)

  n_miss <- map_int(.x = x,
                    .f = function(x) sum(is.na(x = x),
                                         na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- map_int(.x = x,
                      .f = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)))

  pct_unique <- n_unique / nrow(x = x) * 100

  zhaoy_min <- unlist(x = map(.x = x,
                              .f = zhaoy_numeric,
                              fun = "min"))

  zhaoy_max <- unlist(x = map(.x = x,
                              .f = zhaoy_numeric,
                              fun = "max"))

  zhaoy_median <- unlist(x = map(.x = x,
                                 .f = zhaoy_numeric,
                                 fun = "median"))

  zhaoy_mode <- unlist(x = map(.x = x,
                               .f = zhaoy_mode))

  zhaoy_mean <- unlist(x = map(.x = x,
                               .f = zhaoy_numeric,
                               fun = "mean"))

  x <- data.frame(col,
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

  x <- x %>%
    modify_at(.at = c("pct_miss",
                      "pct_unique",
                      "median",
                      "mean"),
              .f = function(x) dplyr::case_when(is.numeric(x = x) == TRUE ~
                                                round(x = x,
                                                      digits = 1)))

  if (nrow(x = x) == 1) {

    dplyr::select(.data = x,
                  -col)

  }

  else if (nrow(x = x) != 1) {

    return(value = x)

  }

}