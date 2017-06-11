#' Convert dates or date-times to Date-class dates
#'
#' Execute as.Date with assumptions about the values of some arguments.
#'
#' In x, leading zeros in single-digit months or days, are optional.
#' format is "%Y-%m-%d".
#' origin is blank.
#' tz is the current time zone.
#'
#' @param x An object in year-month-day format.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' x <- c("1960-1-1", "1960-1-2", "1960-3-31", "1960-7-30")
#' z <- ddt_d(x = x)
#' z

ddt_d <- function(x) {

    x <- as.character(x = x)

    x <- as.Date(x = x,
                 format = "%Y-%m-%d",
                 origin = "",
                 tz = "")

    return(value = x)

}