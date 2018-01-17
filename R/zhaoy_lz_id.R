#' Leading zeros in integer identifiers.
#'
#' @description
#' Include or exclude leading zeros in integer identifiers.
#'
#' @usage
#' zhaoy_lz_id(x, lz)
#'
#' @param x a character, factor, or numeric vector of one up-to-9-digits integer.
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

  if ((is.character(x = x) == TRUE &
      nchar(x = x,
            keepNA = TRUE) == 9 &
      lz == TRUE) |
      (is.character(x = x) == TRUE &
      nchar(x = x,
            keepNA = TRUE) != 9 &
      lz == FALSE)) {

    return(value = x)

  } else {if (is.integer(x = x) == FALSE) {

      x <- as.integer(x = x)

    }

    if (is.integer(x = x) == TRUE &
        lz == TRUE) {

      sprintf(fmt = "%09d",
              x)

    } else if (is.integer(x = x) == TRUE &
               lz == FALSE) {

      as.character(x = x)

    }

  }

}