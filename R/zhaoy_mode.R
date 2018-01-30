#' Statistical mode.
#'
#' @description
#' Calculate the statistical mode.
#'
#' @usage
#' zhaoy_mode(x)
#'
#' @param x a vector, factor, or one or more dates / date-times.
#'
#' @return
#' A length-one vector.
#'
#' If \code{\link{NA}} is the most frequent element in \code{x}, \code{\link{NA}} is returned.
#'
#' If \code{x} has one element, that element is returned.
#'
#' If \code{x} has multiple modes, "s_mode()" is returned.
#'
#' If \code{x} has multiple elements and no mode, "no mode" is returned.
#'
#' @examples
#' zhaoy_mode(x = attenu$dist)

zhaoy_mode <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) |
            is.factor(x = x) |
            is.vector(x = x))

  x_table <- table(x,
                   useNA = "ifany")

  x_max <- max(x_table,
               na.rm = TRUE)

  if (length(x = x) == 1) {

    x_mode <- x

  } else if (length(x = x) > 1 &
             all(x_table == x_max,
                 na.rm = TRUE) == TRUE) {

    x_mode <- "no mode"

  } else if (all(x_table == x_max,
                 na.rm = TRUE) == FALSE) {

    x_mode <- names(x = x_table)[x_table == x_max]

    if (length(x = x_mode) > 1) {

      x_mode <- "s_mode()"

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