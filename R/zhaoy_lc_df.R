#' Convert upper-case English characters to lower-case
#'
#' @description
#' Convert upper-case English characters to lower-case.
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