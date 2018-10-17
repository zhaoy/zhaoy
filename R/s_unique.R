#' Unique Values
#'
#' @description
#' Tabulate counts and percents of unique values, including \code{NA}.
#'
#' @usage
#' s_unique(x)
#'
#' @param x an R object.
#'
#' @return
#' A data-frame with the following variables:
#'
#' value: value name.
#'
#' n: count of value.
#'
#' pct: percent of \code{x} that has the value, rounded to one decimal place.
#'
#' @seealso \code{\link{s_mode} \link{s_s}}
#'
#' @export
#'
#' @examples
#' s_unique(x = attenu$station)

s_unique <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "difftime",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE),
            length(x = x) >= 1)

  n <- table(x,
             useNA = "ifany")

  pct <- prop.table(x = n) * 100

  pct <- round(x = pct,
               digits = 1)

  s_unique <- data.frame(n,
                         pct,
                         row.names = NULL,
                         check.rows = TRUE,
                         check.names = TRUE,
                         fix.empty.names = TRUE,
                         stringsAsFactors = FALSE)

  names(x = s_unique)[names(x = s_unique) == "x"] <- "value"

  names(x = s_unique)[names(x = s_unique) == "Freq"] <- "n"

  names(x = s_unique)[names(x = s_unique) == "Freq.1"] <- "pct"

  s_unique <- subset(x = s_unique,
                     select = c(value,
                                n,
                                pct))

  s_unique$value <- as.character(x = s_unique$value)

  s_order <- order(s_unique$value,
                   decreasing = FALSE,
                   na.last = FALSE)

  s_unique[s_order, ]

}