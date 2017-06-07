#' Convert dates or date-times to Date-class dates
#'
#' Convert dates or date-times to Date-class dates.
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
#' ddt_d(x = Sys.Date())
#' ddt_d(x = Sys.time())

ddt_d <- function(x) {

    x <- as.character(x = x)

    x <- as.Date(x = x,
                 format = "%Y-%m-%d",
                 tz = "")

    return(value = x)

}