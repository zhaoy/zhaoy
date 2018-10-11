#' Statistical mode.
#'
#' @description
#' Compute the statistical mode.
#'
#' @usage
#' zhaoy_s_mode(x)
#'
#' @param x an R object.
#'
#' @return
#' A length-one character vector.
#'
#' If multiple modes exist, "s_mode()" is returned.
#'
#' If the mode is \code{\link{NA}}, \code{\link{NA}} is returned.

zhaoy_s_mode <- function(x) {

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

  } else if (length(x = x_table) > 1 &
             all(x_table == x_max,
                 na.rm = FALSE) == TRUE) {

    x_mode <- "s_mode()"

  } else if (length(x = x_table) > 1 &
             all(x_table == x_max,
                 na.rm = FALSE) == FALSE) {

    x_mode <- names(x = x_table)[x_table == x_max]

    if (length(x = x_mode) > 1) {

      x_mode <- "s_mode()"

    }

  }

  return(value = x_mode)

}