#' Convert to lower-case.
#'
#' @description
#' Convert upper-case letters to lower-case letters.
#'
#' Return numbers, factors, and Date / POSIXlt / POSIXct objects un-changed.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector or factor.
#'
#' @return
#' A vector of the same length as \code{x}.

zhaoy_tolower <- function(x) {

  stopifnot((inherits(x = x,
                      what = c("Date",
                               "POSIXct",
                               "POSIXlt"),
                      which = FALSE) == TRUE |
            is.factor(x = x) == TRUE |
            is.vector(x = x) == TRUE))

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    return(value = x)

  }

}