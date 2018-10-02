#' Convert to lower-case.
#'
#' @description
#' Convert upper-case letters to lower-case letters.
#'
#' Return non-alphabetical objects un-changed.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector.
#'
#' @return
#' A vector of the same length as \code{x}.

zhaoy_tolower <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) == TRUE)

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    return(value = x)

  }

}