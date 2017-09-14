#' Mathematical mode
#'
#' @description
#' Calculate mode.
#'
#' @usage
#' s_mode(x)
#'
#' @param x a column.
#'
#' @return A number.
#'
#' @seealso \code{\link{s_col}} \code{\link{s_tbl}}
#'
#' @export
#'
#' @examples
#' s_mode(x = attenu$dist)

s_mode <- function(x) {

  x_table <- table(x,
                   useNA = "ifany")

  x_max <- max(x_table,
               na.rm = TRUE)

  if (all(x_table == x_max,
          na.rm = TRUE) == TRUE) {

    x_mode <- "none"

  } else {

    x_mode <- names(x = x_table)[x_table == x_max]

    if (is.numeric(x = x) == TRUE) {

      x_mode <- as.numeric(x = x_mode)

    }

  }

  return(value = x_mode)

}