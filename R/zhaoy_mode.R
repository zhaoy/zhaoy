#' Statistical mode
#'
#' @description
#' Calculate the mode.
#'
#' @usage
#' zhaoy_mode(x)
#'
#' @param x a vector.
#'
#' @return A vector.

zhaoy_mode <- function(x) {

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

    }

    else if (length(x = x_mode) == 1 &
             is.numeric(x = x) == TRUE) {

      x_mode <- as.numeric(x = x_mode)

    }

    else if (length(x = x_mode) == 1 &
             is.logical(x = x) == TRUE) {

      x_mode <- as.logical(x = x_mode)

    }

  }

  return(value = x_mode)

}