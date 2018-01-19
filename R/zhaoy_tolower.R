#' Convert to lower-case and retain un-converted.
#'
#' @description
#' Convert upper-case characters and factor levels to lower-case characters.
#'
#' Return non-character vectors and non-alphabetic factors un-changed.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector or factor.
#'
#' @return
#' If \code{x} is a character vector or alphabetic factor, a character vector of the same length as \code{x}.
#'
#' If \code{x} is a non-character vector or non-alphabetic factor, \code{x} un-changed.
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' x <- c("MiXeD", "cAsE", "123")
#' zhaoy_tolower(x = x)

zhaoy_tolower <- function(x) {

  stopifnot(is.factor(x = x) |
            is.vector(x = x))

  case_when(is.character(x = x) == TRUE |
            is.factor(x = x) == TRUE ~ tolower(x = x),
            is.character(x = x) == FALSE &
            is.factor(x = x) == FALSE ~ x)

}