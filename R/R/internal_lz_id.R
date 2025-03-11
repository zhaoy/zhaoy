#' Leading Zeros in Medical Record Numbers (MRNs)
#'
#' @description
#' Include or exclude leading zeros in MRNs.
#'
#' MRNs have 0-8 leading zeros and 1-8 positive integers.
#'
#' @usage
#' internal_lz_id(x, lz)
#'
#' @param x A length-one vector.
#' @param lz Logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A length-one character vector.
#' 
#' @noRd

internal_lz_id <- function(x,
                           lz) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "numeric"),
                     which = FALSE) == TRUE,
            is.list(x = x) == FALSE)

  id_character <- as.character(x = x)

  id_nchar <- nchar(x = id_character,
                    allowNA = FALSE,
                    keepNA = TRUE)

  if (is.character(x = x) == TRUE &&
      ((id_nchar == 9 &&
        lz == TRUE) == TRUE ||
       (id_nchar >= 1 &&
        id_nchar < 9 &&
        lz == FALSE) == TRUE) == TRUE) {

    x

  } else if (id_nchar >= 1 &&
             id_nchar < 9 &&
             lz == TRUE) {

    lz_id <- rep(x = 0,
                 times = 9 - id_nchar)

    lz_id <- paste(lz_id,
                   sep = "",
                   collapse = "")

    paste(lz_id,
          id_character,
          sep = "")

    } else if (lz == FALSE) {

      if (id_nchar == 9) {

        id_character <- as.integer(x = id_character)

        as.character(x = id_character)

      } else if (id_nchar >= 1 &&
                 id_nchar < 9) {

        id_character

      }

    }

  }