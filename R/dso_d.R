#' Convert days-since-origin to Date-class dates
#'
#' Execute as.Date with assumptions about the values of some arguments.
#'
#' format is un-necessary.
#' tz is blank.
#'
#' @param x An object that stores days-since-origin.
#' @param origin An origin date in year-month-day format.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' dso_d(x = 32768, origin = "1900-1-1")

dso_d <- function(x,
                  origin) {

    x <- as.numeric(x = x)

    x <- as.Date(x = x,
                 origin = origin,
                 tz = "")

    return(value = x)

}