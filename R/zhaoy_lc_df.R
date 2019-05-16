#' Character case
#'
#' @description
#' Translate characters from upper to lower case. Leave non-alphabetic characters un-changed.
#'
#' @usage
#' zhaoy_lc_df(x)
#'
#' @param x a vector.
#'
#' @return
#' A vector of the same length as \code{x}.

zhaoy_lc_df <- function(x) {

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    x

  }

}