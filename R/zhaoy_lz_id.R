#' Leading zeros in identifiers.
#'
#' @description
#' Include or exclude leading zeros in non-decimal numeric identifiers that have 1 - 9 digits.
#'
#' @usage
#' zhaoy_lz_id(x, lz)
#'
#' @param x a vector or factor of one non-decimal numeric identifier that has 1 - 9 digits.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A character vector of length one.
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
                  keepNA = TRUE) >= 1,
            nchar(x = as.character(x = x),
                  keepNA = TRUE) <= 9)

  if (is.character(x = x) == TRUE &
      ((nchar(x = as.character(x = x),
              keepNA = TRUE) == 9 &
       lz == TRUE) |
       (nchar(x = as.character(x = x),
              keepNA = TRUE) >= 1 &
        nchar(x = as.character(x = x),
              keepNA = TRUE) < 9 &
       lz == FALSE))) {

    return(value = x)

  } else {

    # The "d" in "%09d" means integers.

    if (is.integer(x = x) == FALSE) {

      x <- as.integer(x = x)

    }

    if (lz == TRUE) {

      sprintf(fmt = "%09d",
              x)

    } else if (lz == FALSE) {

      as.character(x = x)

    }

  }

}