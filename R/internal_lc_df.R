#' Convert upper-case English characters to lower-case
#'
#' @description
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' internal_lc_df(x)
#'
#' @param x A vector.
#'
#' @return
#' A vector of the same length as \code{x}.
#' 
#' @noRd

internal_lc_df <- function(x) {

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    x

    }

}