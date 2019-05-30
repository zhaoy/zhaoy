#' Statistical mode
#'
#' @description
#' Calculate the statistical mode.
#'
#' @usage
#' s_mode(x)
#'
#' @param x a vector.
#'
#' @return
#' A vector.
#'
#' If the mode is missing-data: \code{NA}.
#' 
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' zhaoy::s_mode(x = attenu$dist)

s_mode <- function(x) {

  what <- c("character",
            "integer",
            "logical",
            "numeric")

  class_mode <- inherits(x = x,
                         what = what,
                         which = FALSE)

  data <- zhaoy::s_unique(x = x)

  data <- dplyr::filter(.data = data,
                        is.na(x = value) == TRUE |
                        value != "total")

  data <- dplyr::filter(.data = data,
                        n == max(n,
                                 na.rm = FALSE))

  s_mode <- data$value

  if (class_mode == TRUE) {

    class(x = s_mode) <- class(x = x)

    mode(x = s_mode) <- mode(x = x)

  }

  s_mode

}