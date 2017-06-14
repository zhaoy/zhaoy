#' Date-time conversion to character
#'
#' Function to convert non-character representations of dates and date-times to objects of class "character" representing dates.
#' Executes strftime with assumptions about the values of some arguments.
#'
#' In x, leading zeros in single-digit months or days, are optional.
#'
#' The current time zone is used for the conversion.
#'
#' The format method is "%Y-%m-%d".
#'
#' Does not append the time zone abbreviation to the output.
#'
#' @param x An object in year-month-day format, to be converted.
#'
#' @export
#'
#' @examples
#' x <- c("1960-1-1", "1960-1-2", "1960-3-31", "1960-7-30")
#' z <- dc(x = x)
#' z

dc <- function(x) {

    x <- strftime(x = x,
                  format = "%Y-%m-%d",
                  tz = "",
                  usetz = FALSE)

    return(value = x)

}