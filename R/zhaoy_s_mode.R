#' Statistical mode
#'
#' @description
#' Calculate the statistical mode.
#'
#' @usage
#' zhaoy_s_mode(x)
#'
#' @param x a vector.
#'
#' @return
#' A length-one vector.
#'
#' If the mode is missing-data: \code{NA}.
#'
#' If multiple modes exist: "s_mode()".
#'
#' @importFrom dplyr filter

zhaoy_s_mode <- function(x) {

  data <- zhaoy::s_unique(x = x)

  data <- dplyr::filter(.data = data,
                        is.na(x = value) == TRUE |
                        value != "total")

  data <- dplyr::filter(.data = data,
                        n == max(n,
                                 na.rm = FALSE))

  x <- data$value

  if (length(x = x) == 1) {

    s_mode <- x

  } else if (length(x = x) > 1) {

    s_mode <- "s_mode()"

    }

  s_mode

}