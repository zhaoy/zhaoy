#' Add or subtract leading zeros from medical record numbers (MRNs).
#'
#' Add or subtract leading zeros from medical record numbers (MRNs).
#'
#' With leading zeros, MRNs are 9 digits.
#' When searching for patients by MRN, Provide Enterprise requires 9-digit MRNs.
#'
#' @param x An object containing MRNs.
#' @param lz logical: should leading zeros be added? FALSE subtracts leading zeros.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' mrn(x = 1, lz = TRUE)
#' mrn(x = 000000001, lz = FALSE)

mrn <- function(x,
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