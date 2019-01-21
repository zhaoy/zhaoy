#' Leading Zeros in Medical Record Numbers (MRNs)
#'
#' @description
#' Include or exclude leading zeros in MRNs.
#'
#' MRNs have 0-8 leading zeros and 1-8 positive integers.
#'
#' @usage
#' lz_id(x, lz)
#'
#' @param x a vector of MRNs.
#' @param lz logical: TRUE includes, and FALSE excludes, leading zeros.
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
#' lz_id(x = x, lz = TRUE)
#' lz_id(x = x, lz = FALSE)

lz_id <- function(x,
                  lz) {

  if (lz == TRUE) {

    purrr::map_chr(.x = x,
                   .f = zhaoy_lz_id,
                   lz = TRUE)

  } else if (lz == FALSE) {

    purrr::map_chr(.x = x,
                   .f = zhaoy_lz_id,
                   lz = FALSE)

  }

}