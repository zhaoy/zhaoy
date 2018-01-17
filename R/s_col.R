#' Column-oriented counts and percents.
#'
#' @description
#' Tabulate data as counts and percents of data-frame columns.
#'
#' @usage
#' s_col(x)
#'
#' @param x a data-frame column.
#'
#' @return
#' A base-R data-frame.
#' The "value" column is in ascending order, beginning with any \code{\link{NA}}s.
#' The "pct" column rounds numbers to the nearest integers.
#'
#' @seealso \code{\link{s_mode} \link{s_tbl}}
#'
#' @examples
#' s_col(x = attenu$station)
#'
#' @export

s_col <- function(x) {

  n <- table(x,
             useNA = "ifany")

  pct <- prop.table(x = n) * 100

  pct <- round(x = pct,
               digits = 0)

  x <- data.frame(n,
                  pct,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  x <- x[, c("x",
             "Freq",
             "Freq.1")]

  names(x = x)[names(x = x) == "x"] <- "value"

  names(x = x)[names(x = x) == "Freq"] <- "n"

  names(x = x)[names(x = x) == "Freq.1"] <- "pct"

  x_order <- order(x$value,
                   decreasing = FALSE,
                   na.last = FALSE)

  x[x_order, ]

}