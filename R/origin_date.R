#' Days-since-origin conversion to Date class
#'
#' Function to convert representations of days-since-origin to objects of class "Date" representing dates.
#' Executes as.Date with assumptions about the values of some arguments.
#'
#' format is un-necessary.
#'
#' tz is the current time zone.
#'
#' @param x An object representing days-since-origin, to be converted.
#' @param origin A Date object, or something which can be coerced by as.Date(origin, ...) to such an object.
#'
#' @export
#'
#' @examples
#' origin_date(x = 32768, origin = "1900-1-1")

origin_date <- function(x,
                        origin) {

    x <- as.numeric(x = x)

    x <- as.Date(x = x,
                 origin = origin,
                 tz = "")

    return(value = x)

}