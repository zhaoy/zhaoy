#' Summarize vectors or data-frames
#'
#' @description
#' Tabulate summary statistics of vectors or data-frames, which are made up of vectors.
#'
#' @usage
#' \code{s_vec(x)}
#'
#' @param x a vector or data-frame.
#'
#' @details
#' The contents of the output are:
#'
#' col: column name, displayed only if the input is a data-frame
#'
#' n_miss: number of missing values
#'
#' pct_miss: percent of column that are missing values
#'
#' n_unique: number of unique values
#'
#' pct_unique: percent of column that are unique values
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
#' s_vec(x = airquality$Ozone)

s_vec <- function(x) {

  x <- as.data.frame(x = x,
                     row.names = NULL,
                     stringsAsFactors = FALSE,
                     cut.names = TRUE,
                     fix.empty.names = TRUE)

  col <- names(x = x)

  n_miss <- lapply(X = x,
                   FUN = function(x) sum(is.na(x = x) == TRUE,
                                         na.rm = FALSE))

  n_unique <- lapply(X = x,
                     FUN = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)))

  min <- lapply(X = x,
                FUN = numeric_min)

  max <- lapply(X = x,
                FUN = numeric_max)

  median <- lapply(X = x,
                   FUN = numeric_median)

  mean <- lapply(X = x,
                 FUN = numeric_mean)

  x <- data.frame(col,
                  n_miss,
                  n_unique,
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

    x <- subset(x = x,
                select = -col)

  }

  return(value = x)

}