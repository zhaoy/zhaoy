#' Convert characters, factors, and logicals to lower-case.
#'
#' @description
#' Execute \code{\link{tolower}} on character, factor, and logical vectors.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a character, factor, or logical vector.
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