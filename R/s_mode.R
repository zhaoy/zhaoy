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
#' A vector.
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
#' zhaoy::s_mode(x = attenu$dist)

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
                     which = FALSE) == TRUE,
            is.list(x = x) == FALSE)

  ciln <- inherits(x = x,
                   what = dplyr::combine("character",
                                         "integer",
                                         "logical",
                                         "numeric"),
                   which = FALSE)

  s_unique <- unique(x = x,
                     incomparables = FALSE)

  s_table <- table(x,
                   useNA = "ifany")

  s_max <- max(s_table,
               na.rm = FALSE)

  if (length(x = s_table) == 1) {

    s_mode <- s_unique

  } else if (length(x = s_table) > 1) {

    s_mode <- names(x = s_table)[s_table == s_max]

    if (ciln == TRUE) {

    class(x = s_mode) <- class(x = x)

    mode(x = s_mode) <- mode(x = x)

    }

  }

  s_mode

}