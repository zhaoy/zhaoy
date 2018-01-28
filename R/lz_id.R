#' Leading zeros in identifiers.
#'
#' @description
#' Includes or excludes leading zeros in identifiers that have 1 - 9 digits,
#' of which at least one digit is a positive integer
#' and any remaining digits are either zeros or positive integers.
#'
#' @usage
#' lz_id(x, lz)
#'
#' @param x a vector or factor of identifiers.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A character vector.
#'
#' @export
#'
#' @examples
#' x <- c("1", "000000002")
#' lz_id(x = x, lz = TRUE)
#' lz_id(x = x, lz = FALSE)

lz_id <- function(x,
                  lz) {

  stopifnot(is.factor(x = x) |
            is.vector(x = x))

  if (lz == TRUE) {

    vapply(X = x,
           FUN = zhaoy_lz_id,
           lz = TRUE,
           FUN.VALUE = "a",
           USE.NAMES = FALSE)

  } else if (lz == FALSE) {

    vapply(X = x,
           FUN = zhaoy_lz_id,
           lz = FALSE,
           FUN.VALUE = "a",
           USE.NAMES = FALSE)

  }

}