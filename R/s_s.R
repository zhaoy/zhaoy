#' Summary statistics of data-frames and vectors.
#'
#' @description
#' Tabulate summary statistics of data-frames and vectors.
#'
#' @usage
#' s_s(x)
#'
#' @param x a data-frame or vector.
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' col: column names, displayed only when the input is a data-frame.
#'
#' n_miss: numbers of missing data.
#'
#' pct_miss: numbers of missing data, as percents rounded to the nearest integers.
#'
#' n_unique: numbers of unique values.
#'
#' pct_unique: numbers of unique values, as percents rounded to the nearest integers.
#'
#' min, max, median, mean: \code{\link{NA}} for non-numeric data.
#'
#' mode: \code{\link{NA}} only when \code{\link{NA}} is the most frequent value.
#' When the data has multiple modes, "> 1 mode" is returned.
#' When the data has no mode, "no mode" is returned.
#'
#' @seealso \code{\link{s_cp} \link{s_mode}}
#'
#' @importFrom dplyr case_when select
#' @importFrom purrr map map_int modify_at
#'
#' @export
#'
#' @examples
#' s_s(x = attenu$mag)
#' s_s(x = attenu)

s_s <- function(x) {

  stopifnot(is.data.frame(x = x) |
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

  n_miss <- purrr::map_int(.x = x,
                           .f = function(x) sum(is.na(x = x) == TRUE,
                                                na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- purrr::map_int(.x = x,
                             .f = function(x) length(x = unique(x = x,
                                                     incomparables = FALSE)))

  pct_unique <- n_unique / nrow(x = x) * 100

  zhaoy_min <- unlist(x = purrr::map(.x = x,
                                     .f = zhaoy_s_numeric,
                                     fun = "min"))

  zhaoy_max <- unlist(x = purrr::map(.x = x,
                                     .f = zhaoy_s_numeric,
                                     fun = "max"))

  zhaoy_median <- unlist(x = purrr::map(.x = x,
                                        .f = zhaoy_s_numeric,
                                        fun = "median"))

  zhaoy_mode <- unlist(x = purrr::map(.x = x,
                                      .f = zhaoy_mode))

  zhaoy_mean <- unlist(x = purrr::map(.x = x,
                                      .f = zhaoy_s_numeric,
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

  x <- purrr::modify_at(.x = x,
                        .at = c("pct_miss",
                                "pct_unique"),
                        .f = function(x) dplyr::case_when(is.numeric(x = x) == TRUE ~
                                                          round(x = x,
                                                                digits = 0)))

  if (nrow(x = x) == 1) {

    dplyr::select(.data = x,
                  -col)

  } else if (nrow(x = x) != 1) {

    return(value = x)

  }

}