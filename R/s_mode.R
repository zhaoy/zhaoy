#' Statistical mode
#'
#' @description
#' Calculates mathematical mode.
#'
#' @usage
#' s_mode(x)
#'
#' @param x a column for which a summary is desired.
#'
#' @return An object of type \code{\link{numeric}}.
#'
#' @seealso \code{\link{s_col} \link{s_tbl}}
#'
#' @examples
#' s_mode(x = attenu$dist)
#' 
#' @export

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