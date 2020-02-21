#' @title Convert corrupted scientific-notation characters to fixed-notation numbers
#'
#' @description
#' Convert corrupted scientific-notation characters to fixed-notation numbers.
#'
#' @usage
#' sf_numeric(x)
#'
#' @param x A vector.
#'
#' @return
#' A numeric vector.
#' 
#' @importFrom purrr map_dbl
#'
#' @export
#'
#' @examples
#' sf_numeric(x = "1+003")

sf_numeric <- function(x) {

  purrr::map_dbl(.x = x,
                 .f = internal_sf_numeric)
    
}