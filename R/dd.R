#' Date conversion from character
#'
#' Function to convert character representations to objects of class "Date" representing dates.
#' Executes as.Date with assumptions about the values of some arguments.
#'
#' In x, leading zeros in single-digit months or days, are optional.
#'
#' format is "%Y-%m-%d".
#'
#' origin is "".
#'
#' tz is "".
#'
#' @param x An object in year-month-day format, to be converted.
#'
#' @export
#'
#' @examples
#' # read in date info in format "yyyy-mm-dd"
#' x <- c("1960-1-1", "1960-1-2", "1960-3-31", "1960-7-30")
#' z <- dd(x = x)
#' z

dd <- function(x) {

    x <- as.character(x = x)

    x <- as.Date(x = x,
                 format = "%Y-%m-%d",
                 origin = "",
                 tz = "")

    return(value = x)

}