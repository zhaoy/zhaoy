#' Format medical record numbers (MRNs)
#'
#' Returns a character vector containing medical record numbers (MRNs) that are with or without leading zeros.
#'
#' With leading zeros, MRNs are 9 digits.
#' When searching for patients by MRN, Provide Enterprise requires 9-digit MRNs.
#'
#' @param x an integer, real, or character vector.
#' @param lz logical: should leading zeros be added? FALSE subtracts leading zeros.
#'
#' @export
#'
#' @examples
#' fmrn(x = 1, lz = TRUE)
#' fmrn(x = 000000001, lz = FALSE)

fmrn <- function(x,
                lz) {

    x <- as.numeric(x = x)

    x <- sprintf(fmt = "%09d",
                 x)

    if (lz == TRUE) {
      x <- x
    }

    if (lz == FALSE) {
      x <- as.numeric(x = x)
    }

    return(value = x)

}