#' Statistical Mode
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
#' If no mode exists, the result is "no mode".
#'
#' If the mode is missing-data, the result is \code{NA}.
#'
#' @seealso
#' \code{\link{s_s} \link{s_unique}}
#'
#' @importFrom dplyr combine
#'
#' @export
#'
#' @examples
#' s_mode(x = attenu$dist)

s_mode <- function(x) {

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

  if (length(x = s_table) > 1 &
      s_max == 1) {

    s_mode <- "no mode"

  } else if (length(x = s_table) == 1 &&
             s_max == 1) {

    s_mode <- x

  } else if (length(x = s_table) > 1 &
             s_max > 1) {

    s_mode <- names(x = s_table)[s_table == s_max]

    s_unique <- unique(x = x,
                       incomparables = FALSE)

    s_mode <- s_unique[s_unique %in% s_mode == TRUE]

  }

  s_mode

}