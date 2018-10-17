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

  } else if (length(x = s_table) > 1) {

    s_mode <- names(x = s_table)[s_table == s_max]

  }

  s_mode

}