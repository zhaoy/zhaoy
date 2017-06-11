#' Convert dates or date-times to date strings
#'
#' Execute strftime with assumptions about the values of some arguments.
#'
#' In x, leading zeros in single-digit months or days, are optional.
#' format is "%Y-%m-%d".
#' tz is the current time zone.
#' usetz excludes time zone abbreviations from outputs.
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
#' z <- ddt_ds(x = x)
#' z

ddt_ds <- function(x) {

    x <- strftime(x = x,
                  format = "%Y-%m-%d",
                  tz = "",
                  usetz = FALSE)

    return(value = x)

}