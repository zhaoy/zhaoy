#' @title Parse dates-times.
#'
#' @description
#' Parse dates-times.
#'
#' @usage
#' date_time(x, length, orders)
#'
#' @param x A vector.
#' @param length Length.
#' @param orders Orders.
#'
#' @return
#' A date-time vector.
#'
#' @importFrom lubridate as_date parse_date_time
#' @importFrom stringr str_length
#'
#' @export
#'
#' @examples
#' x <- c("2000-1-1", "12345")
#' date_time(x = x, length = 5, orders = "ymd")

date_time <- function(x,
                      length,
                      orders) {

  x_numeric <- x[! is.na(x = x) &
                 stringr::str_length(string = x) == length]

  if (length(x = x_numeric) >= 1) {

    x[! is.na(x = x) &
      stringr::str_length(string = x) == length] <- x |>
        subset(subset = ! is.na(x = x) &
                        stringr::str_length(string = x) == length) |>
        as.numeric() |>
        lubridate::as_date(origin = "1899-12-30") |>
        as.character()

  }

  lubridate::parse_date_time(x = x,
                             orders = orders,
                             truncated = 0,
                             quiet = FALSE,
                             exact = FALSE,
                             train = TRUE,
                             drop = FALSE)

}