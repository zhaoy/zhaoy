#' Leading zeros in Epic ID numbers.
#'
#' @description
#' Include or exclude leading zeros in Epic ID numbers.
#'
#' Epic ID numbers have 1 - 9 digits,
#' of which at least one digit is a positive integer
#' and any remaining digits are either zeros or positive integers.
#'
#' @usage
#' zhaoy_lz_id(x, lz)
#'
#' @param x a vector of one Epic ID number.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A length-one character vector.

zhaoy_lz_id <- function(x,
                        lz) {

  x_nchar <- nchar(x = as.character(x = x),
                   allowNA = FALSE,
                   keepNA = TRUE)

  stopifnot(is.na(x = as.numeric(x = x)) == FALSE,
            is.vector(x = x) == TRUE,
            length(x = x) == 1,
            x_nchar >= 1,
            x_nchar <= 9)

  if (is.character(x = x) == TRUE &
      ((x_nchar == 9 &
        lz == TRUE) == TRUE |
       (x_nchar >= 1 &
        x_nchar < 9 &
        lz == FALSE) == TRUE) == TRUE) {

    return(value = x)

  } else {

    if (x_nchar >= 1 &
        x_nchar < 9 &
        lz == TRUE) {

      x_lz <- rep(x = 0,
                  times = 9 - x_nchar)

      x_lz <- paste(x_lz,
                    sep = "",
                    collapse = "")

      paste(x_lz,
            x,
            sep = "")

    } else if (lz == FALSE) {

      if (x_nchar == 9) {

      x <- as.integer(x = x)

      }

      as.character(x = x)

    }

  }

}