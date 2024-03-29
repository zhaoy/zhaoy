#' @title Leading Zeros in Medical Record Numbers (MRNs)
#'
#' @description
#' Include or exclude leading zeros in MRNs.
#'
#' MRNs have 0-8 leading zeros and 1-8 positive integers.
#'
#' @usage
#' lz_id(x, lz)
#'
#' @param x A vector.
#' @param lz Logical: TRUE includes, and FALSE excludes, leading zeros.
#'
#' @return
#' A character vector.
#'
#' @importFrom purrr map_chr
#'
#' @export
#'
#' @examples
#' x <- c("1", "000000002")
#' zhaoy::lz_id(x = x, lz = TRUE)
#' zhaoy::lz_id(x = x, lz = FALSE)

lz_id <- function(x,
                  lz) {

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "numeric"),
                     which = FALSE) == TRUE,
            is.list(x = x) == FALSE)

  if (lz == TRUE) {

    purrr::map_chr(.x = x,
                   .f = internal_lz_id,
                   lz = TRUE)

  } else if (lz == FALSE) {

    purrr::map_chr(.x = x,
                   .f = internal_lz_id,
                   lz = FALSE)

  }

}