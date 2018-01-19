#' Counts and percents.
#'
#' @description
#' Tabulate counts and percents of unique values within vectors and factors.
#'
#' @usage
#' s_cp(x)
#'
#' @param x a vector or factor.
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' value: unique values,
#' which begin with \code{\link{NA}} if it is present,
#' then continue in ascending order.
#'
#' n: counts.
#'
#' pct: counts as percents rounded to the nearest integers.
#'
#' @seealso \code{\link{s_mode} \link{s_s}}
#'
#' @export
#'
#' @examples
#' s_cp(x = attenu$station)

s_cp <- function(x) {

  stopifnot(is.factor(x = x) |
            is.vector(x = x))

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

  x <- subset(x = x,
              select = -`x.1`)

  names(x = x)[names(x = x) == "x"] <- "value"

  names(x = x)[names(x = x) == "Freq"] <- "n"

  names(x = x)[names(x = x) == "Freq.1"] <- "pct"

  x$value <- as.character(x = x$value)

  x_order <- order(x$value,
                   decreasing = FALSE,
                   na.last = FALSE)

  x[x_order, ]

}