#' Leading zeros in identifiers.
#'
#' @description
#' Include or exclude leading zeros in non-decimal numeric identifiers that have 1 - 9 digits.
#'
#' @usage
#' lz_id(x, lz)
#'
#' @param x a vector or factor of non-decimal numeric identifiers that each have 1 - 9 digits.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A character vector.
#'
#' @importFrom dplyr case_when
#' @importFrom purrr map_chr
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

  # Convert factors to vectors to by-pass factor-order pit-falls.

  if (is.factor(x = x) == TRUE) {

    x <- as.vector(x = x)

  }

  case_when(lz == TRUE ~ purrr::map_chr(.x = x,
                                        .f = zhaoy_lz_id,
                                        lz = TRUE),
            lz == FALSE ~ purrr::map_chr(.x = x,
                                         .f = zhaoy_lz_id,
                                         lz = FALSE))

}