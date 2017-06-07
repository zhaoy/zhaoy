#' Sub-set tables according to dates
#'
#' Sub-set tables so that dates in one column are in a date range, inclusive.
#'
#' @param tbl A table.
#' @param col A column.
#' @param from Start date in year-month-day format. Leading zeros in single-digit months or days, are optional.
#' @param to End date in year-month-day format. Leading zeros in single-digit months or days, are optional.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples

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