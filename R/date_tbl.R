#' Sub-set tables according to date range
#'
#' @description
#' Sub-set multi-column tables so that one column of dates or date-times complies with a start date and an end date, inclusive.
#'
#' @details
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
#' z <- date_tbl(x = x, col = "date_1", from = "1960-1-1", to = "1960-3-30")
#' z

date_tbl <- function(x,
                     col,
                     from,
                     to) {

  x <- as.data.frame(x,
                     row.names = NULL,
                     stringsAsFactors = FALSE,
                     cut.names = TRUE,
                     fix.empty.names = TRUE)

  col <- which(x = names(x = x) == col)

  x[, col] <- as.Date(x = x[, col],
                      format = "%Y-%m-%d",
                      origin = "",
                      tz = "")

  from <- as.Date(x = from,
                  format = "%Y-%m-%d",
                  origin = "",
                  tz = "")

  to <- as.Date(x = to,
                format = "%Y-%m-%d",
                origin = "",
                tz = "")

  x <- x[x[, col] >= from &
         x[, col] <= to, ]

  return(value = x)

}