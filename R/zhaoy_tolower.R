#' Convert characters and factors to lower-case.
#'
#' @description
#' If data are characters or factors, execute \code{\link{tolower}}.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector.
#'
#' @return a vector.

zhaoy_tolower <- function(x) {

  if (is.character(x = x) == TRUE |
      is.factor(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE &
             is.factor(x = x) == FALSE) {

    return(value = x)

  }

}