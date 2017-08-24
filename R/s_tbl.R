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
#' The output are:
#'
#' col: column name, displayed only if the input is a table
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
#' @seealso \code{\link{s_col}}
#'
#' @export
#'
#' @examples
#' s_tbl(x = attenu)
#' s_tbl(x = attenu$station)

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

  n_miss <- lapply(X = x,
                   FUN = function(x) sum(is.na(x = x) == TRUE,
                                         na.rm = FALSE))

  n_miss <- unlist(x = n_miss,
                   use.names = FALSE)

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- lapply(X = x,
                     FUN = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)))

  n_unique <- unlist(x = n_unique,
                     use.names = FALSE)

  pct_unique <- n_unique / nrow(x = x) * 100

  x_min <- lapply(X = x,
                  FUN = zhaoy_min)

  x_min <- unlist(x = x_min,
                  use.names = FALSE)

  x_max <- lapply(X = x,
                  FUN = zhaoy_max)

  x_max <- unlist(x = x_max,
                  use.names = FALSE)

  x_median <- lapply(X = x,
                     FUN = zhaoy_median)

  x_median <- unlist(x = x_median,
                     use.names = FALSE)

  x_mean <- lapply(X = x,
                   FUN = zhaoy_mean)

  x_mean <- unlist(x = x_mean,
                   use.names = FALSE)

  x_mode <- lapply(X = x,
                   FUN = zhaoy_mode)

  x_mode <- unlist(x = x_mode,
                   use.names = FALSE)

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
                  stringsAsFactors = FALSE)

  x[, c("pct_miss",
        "pct_unique",
        "median",
        "mean")] <- lapply(X = x[, c("pct_miss",
                                     "pct_unique",
                                     "median",
                                     "mean")],
                           FUN = round,
                           digits = 1)

  x[, c("pct_miss",
        "pct_unique",
        "median",
        "mean")] <- as.data.frame(x = x[, c("pct_miss",
                                            "pct_unique",
                                            "median",
                                            "mean")],
                                  row.names = NULL,
                                  stringsAsFactors = FALSE,
                                  cut.names = TRUE,
                                  fix.empty.names = TRUE)

  if (nrow(x = x) == 1) {

    x <- subset(x = x,
                select = -col)

  }

  return(value = x)

}