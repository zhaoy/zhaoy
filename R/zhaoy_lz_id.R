#' Leading Zeros in Epic ID Numbers
#'
#' @description
#' Include or exclude leading zeros in Epic ID numbers.
#'
#' Epic ID numbers have 1-8 positive integers and 0-8 leading zeros.
#'
#' @usage
#' zhaoy_lz_id(x, lz)
#'
#' @param x an Epic ID number.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A length-one character vector.

zhaoy_lz_id <- function(x,
                        lz) {

  id_character <- as.character(x = x)

  id_nchar <- nchar(x = id_character,
                    allowNA = FALSE,
                    keepNA = TRUE)

  id_regex <- strsplit(x = id_character,
                       split = "",
                       fixed = TRUE)

  id_regex <- unlist(x = id_regex)

  id_regex <- grepl(pattern = "\\D",
                    x = id_regex,
                    ignore.case = TRUE,
                    fixed = FALSE)

  id_substring <- substring(text = id_character,
                            first = 1,
                            last = 1)

  stopifnot(id_nchar >= 1,
            id_nchar <= 9,
            ((id_nchar >= 1 &
              id_nchar < 9 &
              id_substring != "0") == TRUE |
             (id_nchar == 9 &
              id_substring == "0") == TRUE),
            id_regex == FALSE,
            inherits(x = x,
                     what = c("character",
                              "integer",
                              "numeric"),
                     which = FALSE),
            is.list(x) == FALSE,
            length(x = x) == 1)

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
            id_character,
            sep = "")

    } else if (lz == FALSE) {

      if(id_nchar == 9 &
         id_substring == "0") {

        lz_id <- as.integer(x = id_character)

        as.character(x = lz_id)

      } else if (id_nchar >= 1 &
                 id_nchar < 9 &
                 id_substring != "0") {

        id_character

      }

    }

  }

}