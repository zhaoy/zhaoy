#' Column summaries
#'
#' @description
#' Tabulates counts and percentages of values that are in columns.
#' Displays the values in ascending order, beginning with any \code{\link{NA}}s.
#'
#' @usage
#' s_col(x)
#'
#' @param x a column for which a summary is desired.
#'
#' @return A table.
#'
#' @seealso \code{\link{s_mode} \link{s_tbl}}
#'
#' @export
#'
#' @examples
#' s_col(x = attenu$station)

s_col <- function(x) {

  stopifnot(is.factor(x = x) == TRUE |
            is.vector(x = x) == TRUE)

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

  x_order <- order(x$value,
                   decreasing = FALSE,
                   na.last = FALSE)

  x[x_order, ]

}