#' Format MRNs with leading zeros
#'
#' Return a vector of medical record numbers (MRNs) that include or exclude leading zeros.
#'
#' @param x a vector of MRNs.
#' @param lz logical: should leading zeros be added? FALSE excludes leading zeros.
#'
#' @details
#' With leading zeros, MRNs are 9 digits.
#' Provide Enterprise requires 9-digit MRNs.
#'
#' @return An integer or character vector.
#'
#' @export
#'
#' @examples
#' lz_id(x = 1, lz = TRUE)
#' lz_id(x = 000000001, lz = FALSE)

lz_id <- function(x,
                   lz) {

  x <- as.integer(x = x)

  if (lz == TRUE) {

    x <- sprintf(fmt = "%09d",
                 x)

  }

  return(value = x)

}