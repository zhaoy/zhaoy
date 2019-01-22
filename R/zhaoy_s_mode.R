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
#' A length-one vector.
#'
#' If no mode exists, the result is "no mode".
#'
#' If the mode is missing-data, the result is \code{NA}.
#'
#' If multiple modes exist, the result is "s_mode()".
#'
#' @importFrom dplyr combine

zhaoy_s_mode <- function(x) {

  stopifnot(inherits(x = x,
                     what = dplyr::combine("character",
                                           "integer",
                                           "logical",
                                           "numeric",
                                           "factor",
                                           "Date",
                                           "difftime",
                                           "POSIXct",
                                           "POSIXlt"),
                     which = FALSE),
            is.list(x = x) == FALSE)

  s_table <- table(x,
                   useNA = "ifany")

  s_max <- max(s_table,
               na.rm = FALSE)

  if (length(x = s_table) > 1 &&
      s_max == 1) {

    s_mode <- "no mode"

  } else if (length(x = s_table) == 1) {

    s_mode <- unique(x = x,
                     incomparables = FALSE)

  } else if (length(x = s_table) > 1 &&
             s_max > 1) {

    s_mode <- names(x = s_table)[s_table == s_max]

    s_unique <- unique(x = x,
                       incomparables = FALSE)

    s_mode <- s_unique[s_unique %in% s_mode == TRUE]

    if (length(x = s_mode) > 1) {

      s_mode <- "s_mode()"

    }

  }

  s_mode

}