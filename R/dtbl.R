#' Sub-set Tables According to a Date Range
#'
#' Sub-set multi-column tables so that a column of dates or date-times complies with a start date and an end date, inclusive.
#'
#' In tbl, leading zeros in single-digit months or days, are optional.
#'
#' @param tbl A multi-column table.
#' @param col A column of dates in year-month-day format.
#' @param from A string representing the start date in year-month-day format.
#' @param to A string representing the end date in year-month-day format.
#'
#' @export
#'
#' @examples
#' x <- data.frame(date_1 = c("1960-1-1", "1960-1-2", "1960-3-31"),
#'                 date_2 = c("1960-1-1", "1960-1-2", "1960-3-31"))
#' z <- dtbl(tbl = x, col = "date_1", from = "1960-1-1", to = "1960-3-30")
#' z

dtbl <- function(tbl,
                 col,
                 from,
                 to) {

    col_index <- which(x = names(x = tbl) == col,
                       arr.ind = FALSE)

    tbl[, col_index] <- dd(x = tbl[, col_index])

    tbl <- tbl[tbl[, col_index] >= from &
               tbl[, col_index] <= to, ]

    return(value = tbl)

}