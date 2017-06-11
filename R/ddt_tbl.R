#' Sub-set tables according to date range
#'
#' Sub-set multi-column tables so that a column of dates complies with a date range, inclusive.
#'
#' In tbl, leading zeros in single-digit months or days, are optional.
#'
#' @param tbl A multi-column table.
#' @param col A column of non-factor dates in year-month-day format.
#' @param from A string storing the start date in year-month-day format.
#' @param to A string storing the end date in year-month-day format.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' x <- data.frame(date_1 = c("1960-1-1", "1960-1-2", "1960-3-31"),
#'                 date_2 = c("1960-1-1", "1960-1-2", "1960-3-31"),
#'                 stringsAsFactors = FALSE)
#' z <- ddt_tbl(tbl = x, col = date_1, from = "1960-1-1", to = "1960-3-30")
#' z

ddt_tbl <- function(tbl,
                    col,
                    from,
                    to) {

    args <- as.list(match.call())

    col <- eval(expr = args$col,
                envir = tbl)

    col <- ddt_d(x = col)

    tbl <- tbl[col >= from &
               col <= to, ]

    return(value = tbl)

}