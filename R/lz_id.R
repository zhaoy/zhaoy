#' Leading zeros in numeric identifiers.
#'
#' @description Includes or excludes leading zeros in numeric identifiers.
#'
#' @usage lz_id(x, lz)
#'
#' @param x A vector of numeric identifiers, each up to 9 digits.
#' @param lz Logical: should leading zeros be included?
#'
#' @return A character vector.
#'
#' @importFrom purrr map
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

    x <- purrr::map(.x = x,
                    .f = zhaoy_lz_id,
                    lz = TRUE)

  } else if (lz == FALSE) {

    x <- purrr::map(.x = x,
                    .f = zhaoy_lz_id,
                    lz = FALSE)

  }

  unlist(x = x)

}