#' Date-time conversion to character
#'
#' Function to convert non-character representations to objects of class "\code{character}" representing dates.
#' Executes \code{strftime} with assumptions about the values of some arguments.
#'
#' In x, leading zeros in single-digit months or days, are optional.
#'
#' The current time zone is used for the conversion.
#'
#' The \code{format} method is "\code{%Y-%m-%d}".
#'
#' The time zone abbreviation is not appended to the output.
#'
#' @param x An object in year-month-day format, to be converted.
#'
#' @return Dates of class "\code{character}", in year-month-day format.
#'
#' @seealso \code{\link{dd}}
#'
#' @examples
#' # read in date info in format "yyyy-mm-dd"
#' x <- c("1960-1-1", "1960-1-2", "1960-3-31", "1960-7-30")
#' z <- dc(x = x)
#' z
#'
#' @export

dc <- function(x) {

    x <- strftime(x = x,
                  tz = "",
                  format = "%Y-%m-%d",
                  usetz = FALSE)

    return(value = x)

}