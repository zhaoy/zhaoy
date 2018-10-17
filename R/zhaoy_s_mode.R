#' Statistical Mode
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
#' If the mode is \code{\link{NA}}, \code{\link{NA}} is returned.
#'
#' If multiple modes exist, "s_mode()" is returned.

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
                     which = FALSE),
            length(x = x) >= 1)

  s_table <- table(x,
                   useNA = "ifany")

  s_max <- max(s_table,
               na.rm = FALSE)

  if (length(x = s_table) == 1) {

    s_mode <- x

    if (is.character(x = s_mode) == FALSE) {

      s_mode <- as.character(x = s_mode)

    }

  } else if (length(x = s_table) > 1 &
             all(s_table == s_max,
                 na.rm = FALSE) == TRUE) {

    s_mode <- "s_mode()"

  } else if (length(x = s_table) > 1 &
             all(s_table == s_max,
                 na.rm = FALSE) == FALSE) {

    s_mode <- names(x = s_table)[s_table == s_max]

    if (length(x = s_mode) > 1) {

      s_mode <- "s_mode()"

    }

  }

  s_mode

}