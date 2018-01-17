#' Counts and percents.
#'
#' @description
#' Tabulate counts and percents of vectors.
#'
#' @usage
#' s_cp(x)
#'
#' @param x a character, factor, logical, or numeric vector.
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' value: unique values, in ascending order beginning with any \code{\link{NA}}s.
#'
#' n: counts.
#'
#' pct: percents rounded to the nearest integers.
#'
#' @seealso \code{\link{s_mode} \link{s_s}}
#'
#' @export
#'
#' @examples
#' s_cp(x = attenu$station)

s_cp <- function(x) {

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