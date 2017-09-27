#' Leading zeros in medical record numbers (MRNs)
#'
#' Includes or excludes leading zeros in MRNs.
#'
#' @param x a vector of MRNS, each up to 9 digits.
#' @param lz logical: should leading zeros be included?.
#'
#' @details
#' Provide Enterprise requires 9-digit MRNs.
#' 
#' MRNs that include leading zeros, are 9 digits.
#'
#' @return
#' When \code{lz = TRUE}, a vector of type \code{\link{character}}.
#' 
#' When \code{lz = FALSE}, a vector of type \code{\link{integer}}.
#'
#' @export
#'
#' @examples
#' lz_id(x = 1, lz = TRUE)
#' lz_id(x = 000000001, lz = FALSE)

lz_id <- function(x,
                  lz) {
    
  if (is.integer(x = x) == FALSE) {
    
    x <- as.integer(x = x)
    
  }
  
  if (lz == TRUE) {
    
    sprintf(fmt = "%09d",
            x)
    
  } else if (lz == FALSE) {
    
    return(value = x)
    
  }
  
}