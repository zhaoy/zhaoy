#' Convert to lower-case.
#'
#' @description
#' Convert characters from upper to lower case.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector.
#'
#' @details
#' Non-alphabetic characters and non-character objects are left un-changed.
#'
#' @return
#' A vector of the same length as \code{x}.

zhaoy_tolower <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE),
            length(x = x) >= 1)

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    return(value = x)

  }

}