#' Statistical mode.
#'
#' @description
#' Compute the statistical mode.
#'
#' @usage
#' s_mode(x)
#'
#' @param x a vector.
#'
#' @return
#' A character vector.
#'
#' If the mode is \code{\link{NA}}, \code{\link{NA}} is returned.
#'
#' If no mode exists, "no mode" is returned.
#'
#' @seealso
#' \code{\link{s_s} \link{s_unique}}
#'
#' @export
#'
#' @examples
#' s_mode(x = attenu$dist)

s_mode <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) == TRUE,
            length(x = x) >= 1)

  x_table <- table(x,
                   useNA = "ifany")

  x_max <- max(x_table,
               na.rm = FALSE)

  if (length(x = x) == 1) {

    x_mode <- x

  } else if (length(x = x) > 1 &
             all(x_table == x_max,
                 na.rm = FALSE) == TRUE) {

    x_mode <- "no mode"

  } else if (length(x = x) > 1 &
             all(x_table == x_max,
                 na.rm = FALSE) == FALSE) {

    x_mode <- names(x = x_table)[x_table == x_max]

  }

  return(value = x_mode)

}