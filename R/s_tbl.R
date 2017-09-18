#' Column or table summaries
#'
#' @description
#' Tabulate summary statistics of columns or tables.
#'
#' @usage
#' s_tbl(x)
#'
#' @param x a column or table for which a summary is desired.
#'
#' @return A table.
#'
#' @details
#' The outputs are:
#'
#' col: column name, displayed only if input is a table
#'
#' n_miss: number of missing data
#'
#' pct_miss: percent of column that are missing data
#'
#' n_unique: number of unique values
#'
#' pct_unique: percent of column that are unique values
#'
#' min, max, median, mode, mean
#'
#' @seealso \code{\link{s_col} \link{s_mode}}
#'
#' @import purrr
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
                       col.names = names(x = x),
                       fix.empty.names = TRUE)

  }

  col <- names(x = x)

  n_miss <- map_int(.x = x,
                    .f = function(x) sum(is.na(x = x) == TRUE,
                                         na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100
  
  n_unique <- map_int(.x = x,
                      .f = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)))

  pct_unique <- n_unique / nrow(x = x) * 100

  x_min <- map(.x = x,
               .f = zhaoy_min)
  
  x_max <- map(.x = x,
               .f = zhaoy_max)
  
  x_median <- map(.x = x,
                  .f = zhaoy_median)
  
  x_mode <- map(.x = x,
                .f = zhaoy_mode)
  
  x_mean <- map(.x = x,
                .f = zhaoy_mean)

  x <- data.frame(col,
                  n_miss,
                  pct_miss,
                  n_unique,
                  pct_unique,
                  min = x_min,
                  max = x_max,
                  median = x_median,
                  mode = x_mode,
                  mean = x_mean,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE) %>%
    map(.f = unlist)

  x[, c("pct_miss",
        "pct_unique",
        "median",
        "mean")] <- map_dfc(.x = x[, c("pct_miss",
                                      "pct_unique",
                                      "median",
                                      "mean")],
                            .f = round,
                            digits = 1)

  if (nrow(x = x) == 1) {

    x <- subset(x = x,
                select = -col)

  }

  return(value = x)

}