#' Statistical mode.
#'
#' @description
#' Calculate the statistical mode.
#'
#' @usage
#' zhaoy_mode(x)
#'
#' @param x a vector or factor.
#'
#' @return
#' A vector of length one.
#'
#' If \code{\link{NA}} is the most frequent value in \code{x}, "NA" or "<NA>" is returned.
#'
#' If \code{x} has multiple modes, "> 1 mode" is returned.
#'
#' If \code{x} has no mode, "no mode" is returned.
#'
#' @examples
#' zhaoy_mode(x = attenu$dist)

zhaoy_mode <- function(x) {

  stopifnot(is.factor(x = x) |
            is.vector(x = x))

  x_table <- table(x,
                   useNA = "ifany")

  x_max <- max(x_table,
               na.rm = TRUE)

  if (all(x_table == x_max,
          na.rm = TRUE) == TRUE) {

    x_mode <- "no mode"

  } else if (all(x_table == x_max,
                 na.rm = TRUE) == FALSE) {

    x_mode <- names(x = x_table)[x_table == x_max]

    if (length(x = x_mode) > 1) {

      x_mode <- "> 1 mode"

    } else if (length(x = x_mode) == 1) {

      if (is.numeric(x = x) == TRUE) {

        x_mode <- as.numeric(x = x_mode)

      } else if (is.logical(x = x) == TRUE) {

        x_mode <- as.logical(x = x_mode)

      }

    }

  }

  return(value = x_mode)

}