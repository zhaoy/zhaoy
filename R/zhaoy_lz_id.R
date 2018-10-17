#' Leading Zeros in Epic ID Numbers
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

  id_nchar <- nchar(x = as.character(x = x),
                    allowNA = FALSE,
                    keepNA = TRUE)

  stopifnot(inherits(x = x,
                     what = c("data.frame",
                              "list"),
                     which = FALSE) == FALSE,
            is.na(x = as.numeric(x = x)) == FALSE,
            length(x = x) == 1,
            id_nchar >= 1,
            id_nchar <= 9)

  if (is.character(x = x) == TRUE &
      ((id_nchar == 9 &
        lz == TRUE) == TRUE |
       (id_nchar >= 1 &
        id_nchar < 9 &
        lz == FALSE) == TRUE) == TRUE) {

    x

  } else {

    if (id_nchar >= 1 &
        id_nchar < 9 &
        lz == TRUE) {

      lz_id <- rep(x = 0,
                   times = 9 - id_nchar)

      lz_id <- paste(lz_id,
                     sep = "",
                     collapse = "")

      paste(lz_id,
            x,
            sep = "")

    } else if (lz == FALSE) {

      if (id_nchar == 9) {

      lz_id <- as.integer(x = x)

      }

      as.character(x = lz_id)

    }

  }

}