#' Leading zeros in identifiers.
#'
#' @description
#' Includes or excludes leading zeros in number-only identifiers of 1 - 9 digits.
#'
#' @usage
#' zhaoy_lz_id(x, lz)
#'
#' @param x a vector or factor of one numbers-only identifier that has 1 - 9 digits.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A length-one character vector.
#'
#' @examples
#' zhaoy_lz_id(x = 1, lz = TRUE)
#' zhaoy_lz_id(x = "000000001", lz = FALSE)

zhaoy_lz_id <- function(x,
                        lz) {

  stopifnot((is.factor(x = x) |
             is.vector(x = x)),
            length(x = x) == 1,
            nchar(x = as.character(x = x),
                  type = "chars",
                  allowNA = FALSE,
                  keepNA = TRUE) >= 1,
            nchar(x = as.character(x = x),
                  type = "chars",
                  allowNA = FALSE,
                  keepNA = TRUE) <= 9)

  if (is.character(x = x) == TRUE &
      ((nchar(x = x,
              type = "chars",
              allowNA = FALSE,
              keepNA = TRUE) == 9 &
        lz == TRUE) |
       (nchar(x = x,
              type = "chars",
              allowNA = FALSE,
              keepNA = TRUE) >= 1 &
        nchar(x = x,
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
                     type = "chars",
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