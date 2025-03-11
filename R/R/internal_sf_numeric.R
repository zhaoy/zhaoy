#' @title Convert corrupted scientific-notation characters to fixed-notation numbers
#'
#' @description
#' Convert corrupted scientific-notation characters to fixed-notation numbers.
#'
#' @usage
#' internal_sf_numeric(x)
#'
#' @param x A vector.
#'
#' @return
#' A numeric vector.

internal_sf_numeric <- function(x) {
  
  if (grepl(pattern = "((\\d)+)[+]((\\d)+)$",
            x = x,
            fixed = FALSE) == TRUE) {
    
    x <- strsplit(x = x,
                  split = "[+]",
                  fixed = FALSE)
    
    x <- c(x,
           recursive = TRUE)
    
    x <- paste(x[1],
               "e+",
               x[2],
               sep = "")
    
  }
  
  as.numeric(x = x)
  
}