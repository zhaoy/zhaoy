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

  n_miss <- unlist(x = n_miss,
                   recursive = TRUE,
                   use.names = FALSE)

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- lapply(X = x,
                     FUN = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)))

  n_unique <- unlist(x = n_unique,
                     recursive = TRUE,
                     use.names = FALSE)

  pct_unique <- n_unique / nrow(x = x) * 100

  min <- lapply(X = x,
                FUN = numeric_min)

  min <- unlist(x = min,
                recursive = TRUE,
                use.names = FALSE)

  max <- lapply(X = x,
                FUN = numeric_max)

  max <- unlist(x = max,
                recursive = TRUE,
                use.names = FALSE)

  median <- lapply(X = x,
                   FUN = numeric_median)

  median <- unlist(x = median,
                   recursive = TRUE,
                   use.names = FALSE)

  mean <- lapply(X = x,
                 FUN = numeric_mean)

  mean <- unlist(x = mean,
                 recursive = TRUE,
                 use.names = FALSE)

  mode <- lapply(X = x,
                 FUN = zhaoy_mode)

  mode <- unlist(x = mode,
                 recursive = TRUE,
                 use.names = FALSE)

  x <- data.frame(col,
                  n_miss,
                  pct_miss,
                  n_unique,
                  pct_unique,
                  min,
                  max,
                  median,
                  mean,
                  mode,
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
                           FUN = zhaoy_round)

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