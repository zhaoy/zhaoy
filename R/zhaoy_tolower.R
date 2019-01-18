#' Character Translation
#'
#' @description
#' Translate characters from upper to lower case.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector.
#'
#' @details
#' Non-alphabetic characters and non-character vectors are left un-changed.
#'
#' @return
#' A vector of the same length as \code{x}.
#'
#' @importFrom dplyr combine

zhaoy_tolower <- function(x) {

  stopifnot(inherits(x = x,
                     what = dplyr::combine("character",
                                           "integer",
                                           "logical",
                                           "numeric",
                                           "factor",
                                           "Date",
                                           "difftime",
                                           "POSIXct",
                                           "POSIXlt"),
                     which = FALSE),
            is.list(x = x) == FALSE)

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    x

  }

}