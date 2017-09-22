#' Leading zeros in medical record numbers (MRNs)
#'
#' Includes or excludes leading zeros in MRNs.
#'
#' @param x one or more MRNs.
#' @param lz logical: should leading zeros be included? FALSE excludes leading zeros.
#'
#' @details
#' With leading zeros, MRNs are 9 digits.
#' 
#' Provide Enterprise requires 9-digit MRNs.
#'
#' @return When \code{lz = TRUE}, an object of type \code{\link{character}}. \code{When lz = FALSE}, an object of type \code{\link{integer}}.
#'
#' @export
#'
#' @examples
#' lz_id(x = 1, lz = TRUE)
#' lz_id(x = 000000001, lz = FALSE)

lz_id <- function(x,
                  lz) {

  if (lz == TRUE) {
    
    sprintf(fmt = "%09d",
            x)

  } else {
    
    as.integer(x = x)
    
  }

}