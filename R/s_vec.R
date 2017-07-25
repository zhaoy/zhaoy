#' Summarize vectors or data-frames.
#'
#' @description
#' Tabulate summary statistics of vectors or data-frames, which are made up of vectors.
#'
#' @usage
#' \code{s_vec(x)}
#'
#' @param x a data-frame.
#'
#' @details
#' The contents of the output are:
#'
#' col: column name
#'
#' n_miss: number of missing values
#'
#' pct_miss: missing values as percent of column
#'
#' n_unique: number of unique values
#'
#' pct_unique: unique values as percent of column
#'
#' min, max, median, mean
#'
#' @return A data-frame.
#'
#' @seealso \code{\link{s_value}}
#'
#' @export
#'
#' @examples
#' s_vec(x = airquality)

s_vec <- function(x) {

  if (is.vector(x = x) == TRUE) {

    x <- as.data.frame(x = x,
                       row.names = NULL,
                       stringsAsFactors = FALSE,
                       cut.names = TRUE,
                       fix.empty.names = TRUE)

  }

  col <- names(x = x)

  n_miss <- apply(X = x,
                  MARGIN = 2,
                  FUN = function(x) sum(is.na(x = x) == TRUE,
                                        na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- apply(X = x,
                    MARGIN = 2,
                    FUN = function(x) length(x = unique(x = x,
                                                        incomparables = FALSE)))

  pct_unique <- n_unique / nrow(x = x) * 100

  min <- apply(X = x,
               MARGIN = 2,
               FUN = numeric_min)

  max <- apply(X = x,
               MARGIN = 2,
               FUN = numeric_max)

  median <- apply(X = x,
                  MARGIN = 2,
                  FUN = numeric_median)

  mean <- apply(X = x,
                MARGIN = 2,
                FUN = numeric_mean)

  x <- data.frame(col,
                  n_miss,
                  pct_miss,
                  n_unique,
                  pct_unique,
                  min,
                  max,
                  median,
                  mean,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  if (nrow(x = x) == 1) {

    subset(x = x,
           select = -col)

  } else {

    x

  }

}