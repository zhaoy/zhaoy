#' Statistical mode.
#'
#' @description
#' Compute the statistical mode.
#'
#' @usage
#' s_mode(x)
#'
#' @param x an R object.
#'
#' @return
#' A character vector.
#'
#' If the mode is \code{\link{NA}}, \code{\link{NA}} is returned.
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
                              "difftime",
                              "POSIXct",
                              "POSIXlt"),
                     which = FALSE) == TRUE,
            length(x = x) >= 1)

  x_table <- table(x,
                   useNA = "ifany")

  x_max <- max(x_table,
               na.rm = FALSE)

  if (length(x = x_table) == 1) {

    x_mode <- x

    if (is.character(x = x_mode) == FALSE) {

      x_mode <- as.character(x = x_mode)

    }

  } else if (length(x = x_table) > 1) {

    x_mode <- names(x = x_table)[x_table == x_max]

  }

  return(value = x_mode)

}