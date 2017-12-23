#' Leading zeros in numeric identifiers.
#'
#' @description
#' Includes or excludes leading zeros in numeric identifiers.
#'
#' @usage
#' lz_id(x, lz)
#'
#' @param x A vector of numeric identifiers, each up to 9 digits.
#' @param lz Logical: should leading zeros be included?
#'
#' @return
#' A character vector.
#'
#' @export
#'
#' @examples
#' lz_id(x = 1, lz = TRUE)
#' lz_id(x = 000000001, lz = FALSE)

lz_id <- function(x,
                  lz) {

  if ((is.character(x = x) == TRUE &
       nchar(x = x,
             keepNA = NA) == 9 &
       lz == TRUE) |
      (is.character(x = x) == TRUE &
       nchar(x = x,
             keepNA = NA) != 9 &
       lz == FALSE)) {

    return(value = x)

  } else {

    if (is.integer(x = x) == FALSE) {

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