#' Statistical mode.
#'
#' @description
#' Calculate the statistical mode.
#'
#' @usage
#' s_mode(x)
#'
#' @param x a vector or factor.
#'
#' @return
#' A vector.
#'
#' If \code{x} has no mode, "no mode" is returned.
#'
#' @seealso
#' \code{\link{s_cp} \link{s_s}}
#'
#' @export
#'
#' @examples
#' s_mode(x = attenu$dist)

s_mode <- function(x) {

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

    if (is.numeric(x = x) == TRUE) {

      x_mode <- as.numeric(x = x_mode)

    } else if (is.logical(x = x) == TRUE) {

      x_mode <- as.logical(x = x_mode)

    }

  }

  return(value = x_mode)

}