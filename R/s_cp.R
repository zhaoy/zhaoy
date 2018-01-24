#' Counts and percents.
#'
#' @description
#' Tabulate counts and percents of unique values within vectors, factors, and dates / date-time(s).
#'
#' @usage
#' s_cp(x)
#'
#' @param x a vector, factor, or one or more dates / date-time(s).
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' value: unique values,
#' which begin with "<NA>" if \code{\link{NA}} is present,
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

  stopifnot(inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
            is.factor(x = x) |
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

  names(x = x)[names(x = x) == "x"] <- "value"

  names(x = x)[names(x = x) == "Freq"] <- "n"

  names(x = x)[names(x = x) == "Freq.1"] <- "pct"

  x <- subset(x = x,
              select = c(value:n,
                         pct))

  x$value <- as.character(x = x$value)

  x_order <- order(x$value,
                   decreasing = FALSE,
                   na.last = FALSE)

  x[x_order, ]

}