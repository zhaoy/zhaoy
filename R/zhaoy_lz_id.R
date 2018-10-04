#' Leading zeros in Epic ID numbers.
#'
#' @description
#' Include or exclude leading zeros in Epic ID numbers.
#'
#' Epic ID numbers have 1 - 9 digits,
#' of which one digit is a positive integer
#' and any remaining digits are either zeros or positive integers.
#'
#' @usage
#' zhaoy_lz_id(x, lz)
#'
#' @param x a vector or factor of one Epic ID number.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A length-one character vector.

zhaoy_lz_id <- function(x,
                        lz) {

  stopifnot((is.factor(x = x) == TRUE |
             is.vector(x = x) == TRUE),
            length(x = x) == 1,
            nchar(x = as.character(x = x),
                  allowNA = FALSE,
                  keepNA = TRUE) >= 1,
            nchar(x = as.character(x = x),
                  allowNA = FALSE,
                  keepNA = TRUE) <= 9)

  if (is.character(x = x) == TRUE &
      ((nchar(x = as.character(x = x),
              allowNA = FALSE,
              keepNA = TRUE) == 9 &
        lz == TRUE) |
       (nchar(x = as.character(x = x),
              allowNA = FALSE,
              keepNA = TRUE) >= 1 &
        nchar(x = as.character(x = x),
              type = "chars",
              allowNA = FALSE,
              keepNA = TRUE) < 9 &
        lz == FALSE))) {

    return(value = x)

  } else {

    if (is.character(x = x) == FALSE) {

      x <- as.character(x = x)

    }

    x_nchar <- nchar(x = x,
                     allowNA = FALSE,
                     keepNA = TRUE)

    if ((9 - x_nchar) >= 1 &
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

      x <- as.integer(x = x)

      as.character(x = x)

    }

  }

}