#' Sub-set tables according to date range
#'
#' Sub-set multi-column tables so that one column of dates or date-times complies with a start date and an end date, inclusive.
#'
#' In x, leading zeros in single-digit months or days, are optional.
#'
#' @param x A multi-column table.
#' @param col A column of dates that are in year-month-day format.
#' @param from A start date in year-month-day format.
#' @param to An end date in year-month-day format.
#'
#' @export
#'
#' @examples
#' x <- data.frame(date_1 = c("1960-1-1", "1960-1-2", "1960-3-31"),
#'                 date_2 = c("1960-1-1", "1960-1-2", "1960-3-31"))
#' z <- table_date(x = x, col = "date_1", from = "1960-1-1", to = "1960-3-30")
#' z

table_date <- function(x,
                 col,
                 from,
                 to) {

    x <- data.frame(x,
                    row.names = NULL,
                    check.rows = TRUE,
                    check.names = TRUE,
                    fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)

    col_index <- which(x = names(x = x) == col,
                       arr.ind = FALSE,
                       useNames = FALSE)

    x[, col_index] <- zhaoy::date_date(x = x[, col_index])

    from <- zhaoy::date_date(x = from)

    to <- zhaoy::date_date(x = to)

    x <- x[x[, col_index] >= from &
           x[, col_index] <= to, ]

    return(value = x)

}