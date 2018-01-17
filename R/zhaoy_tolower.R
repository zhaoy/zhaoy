#' Convert characters and factors to lower-case.
#'
#' @description
#' Execute \code{\link{tolower}} on character and factor vectors.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a character or factor vector.
#'
#' @return
#' A character or factor vector of the same length as \code{x}.
#'
#' @examples
#' x <- c("MiXeD", "cAsE", "123")
#' zhaoy_tolower(x = x)

zhaoy_tolower <- function(x) {

  if (is.character(x = x) == TRUE |
    is.factor(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE &
    is.factor(x = x) == FALSE) {

    return(value = x)

  }

}