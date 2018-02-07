#' Convert to lower-case.
#'
#' @description
#' Convert upper-case characters and factor levels to lower-case characters.
#'
#' Return non-alphabetic vectors and factors, and Date / POSIXlt / POSIXct objects, un-changed.
#'
#' @usage
#' zhaoy_tolower(x)
#'
#' @param x a vector, factor, or one or more Date / POSIXlt / POSIXct objects.
#'
#' @return
#' If \code{x} is an alphabetic vector or factor, a character vector of the same length as \code{x}.
#'
#' Otherwise, \code{x} un-changed.
#'
#' @examples
#' x <- c("MiXeD", "cAsE", "123")
#' zhaoy_tolower(x = x)

zhaoy_tolower <- function(x) {

  stopifnot((inherits(x = x,
                      what = c("Date",
                               "POSIXct",
                               "POSIXlt"),
                      which = FALSE) |
             is.factor(x = x) |
             is.vector(x = x)),
            length(x = x) >= 1)

  if (is.character(x = x) == TRUE |
      is.factor(x = x) == TRUE) {

    tolower(x = x)

  } else if (is.character(x = x) == FALSE &
             is.factor(x = x) == FALSE) {

    return(value = x)

  }

}