#' Convert dates or date-times to strings of dates.
#'
#' Convert dates or date-times to strings of dates.
#'
#' @param x An object in which values are in year-month-day format.
#' Leading zeros in single-digit months or days, are optional.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' ddt_s(x = Sys.Date())
#' ddt_s(x = Sys.time())

ddt_s <- function(x) {

    x <- as.character(x = x)

    x <- strftime(x = x,
                  tz = "",
                  format = "%Y-%m-%d",
                  usetz = FALSE)

    return(value = x)

}