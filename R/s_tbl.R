#' Column or table summaries
#'
#' @description
#' Tabulates summary statistics of columns or tables.
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
#' col: column name, displayed only when input is a table
#'
#' n_miss: number of missing data
#'
#' pct_miss: missing data as percent of column
#'
#' n_unique: number of unique values
#'
#' pct_unique: unique values as percent of column
#'
#' min, max, median, mean: \code{\link{NA}} when column is non-numeric
#' 
#' mode: NA only when NA is the most frequent value in column
#'
#' @seealso \code{\link{s_col} \link{s_mode}}
#'
#' @import purrr
#' @importFrom dplyr case_when
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
                    .f = function(x) sum(is.na(x = x) == TRUE,
                                         na.rm = FALSE))
  
  n_unique <- map_int(.x = x,
                      .f = function(x) length(x = unique(x = x,
                                                         incomparables = FALSE)))

  x <- data.frame(col,
                  n_miss,
                  pct_miss = n_miss / nrow(x = x) * 100,
                  n_unique,
                  pct_unique = n_unique / nrow(x = x) * 100,
                  min = x %>% map(.f = zhaoy_min) %>% unlist,
                  max = x %>% map(.f = zhaoy_max) %>% unlist,
                  median = x %>% map(.f = zhaoy_median) %>% unlist,
                  mode = x %>% map(.f = zhaoy_mode) %>% unlist,
                  mean = x %>% map(.f = zhaoy_mean) %>% unlist,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)
  
  x[, c("pct_miss",
        "pct_unique")] <- x[, c("pct_miss",
                                "pct_unique")] %>%
    map(.f = round,
        digits = 1) %>%
    as.data.frame(row.names = NULL,
                  stringsAsFactors = FALSE,
                  cut.names = TRUE,
                  col.names = c("pct_miss",
                                "pct_unique"),
                  fix.empty.names = TRUE)
  
  x[, c("median",
        "mean")] <- x[, c("median",
                          "mean")] %>%
    map(.f = function(x) dplyr::case_when(is.numeric(x = x) == TRUE ~ 
                                          round(x = x,
                                                digits = 1))) %>%
    as.data.frame(row.names = NULL,
                  stringsAsFactors = FALSE,
                  cut.names = TRUE,
                  col.names = c("median",
                                "mean"),
                  fix.empty.names = TRUE)
  
  if (nrow(x = x) == 1) {
    
    subset(x  = x,
           select = -col)
    
  } else {
    
    return(value = x)
    
  }

}