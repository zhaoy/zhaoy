#' Character Translation
#'
#' @description
#' Translate characters from upper to lower case.
#'
#' @usage
#' zhaoy_lc_df(x)
#'
#' @param x a vector.
#'
#' @return
#' A vector of the same length as \code{x}.
#'
#' Non-alphabetic characters and non-character vectors are left un-changed.
#'
#' @importFrom dplyr combine

zhaoy_lc_df <- function(x) {

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
                     which = FALSE) == TRUE,
            is.list(x = x) == FALSE)

  if (is.character(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE) {

    x

  }

}