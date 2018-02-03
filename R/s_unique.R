#' Summarize unique elements.
#'
#' @description
#' Tabulate counts and percents of unique elements within vectors, factors, and dates / date-time(s).
#'
#' @usage
#' s_unique(x)
#'
#' @param x a vector, factor, or one or more dates / date-time(s).
#'
#' @return
#' A base-R data-frame with the following columns:
#'
#' element: unique elements,
#' beginning with "<NA>" if \code{\link{NA}} is present,
#' then continuing in ascending order.
#'
#' n: counts.
#'
#' pct: counts as percents of \code{x}, rounded to one decimal place.
#'
#' @seealso \code{\link{s_mode} \link{s_s}}
#'
#' @export
#'
#' @examples
#' s_unique(x = attenu$station)

s_unique <- function(x) {

  stopifnot((inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
             is.factor(x = x) |
             is.vector(x = x)),
            length(x = x) >= 1)

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

  names(x = x)[names(x = x) == "x"] <- "element"

  names(x = x)[names(x = x) == "Freq"] <- "n"

  names(x = x)[names(x = x) == "Freq.1"] <- "pct"

  x <- subset(x = x,
              select = c(element:n,
                         pct))

  x$element <- as.character(x = x$element)

  x_order <- order(x$element,
                   decreasing = FALSE,
                   na.last = FALSE)

  x[x_order, ]

}