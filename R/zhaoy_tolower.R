#' Convert only character or factor data to lower-case.
#'
#' @description
#' Execute \code{\link{tolower}} if data is \code{\link{character}} or \code{\link{factor}}.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a character or factor vector.
#'
#' @return A character or factor vector.

zhaoy_tolower <- function(x) {

  if (is.character(x = x) == TRUE |
      is.factor(x = x) == TRUE) {

    tolower(x = x)

  } else {

    return(value = x)

  }

}