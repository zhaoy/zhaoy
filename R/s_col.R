#' Column-value summaries
#'
#' @description
#' Tabulate counts and percentages of column values.
#' Displays column values in ascending order, beginning with any NAs.
#'
#' @usage
#' s_col(x)
#'
#' @param x a column for which a summary of values is desired.
#'
#' @return A table.
#'
#' @seealso \code{\link{s_tbl}}
#'
#' @export
#'
#' @examples
#' s_col(x = attenu$station)

s_col <- function(x) {

  stopifnot(is.factor(x = x) | is.vector(x = x))

  n <- table(x,
             useNA = "ifany")

  pct <- prop.table(x = n) * 100

  pct <- round(x = pct,
               digits = 1)

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

  x[order(x$value,
          decreasing = FALSE,
          na.last = FALSE), ]

}