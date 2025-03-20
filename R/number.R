#' @title Convert data-points to numbers
#'
#' @description Convert data-points to numbers.
#'
#' @usage number(data, var)
#'
#' @param data Data-frame.
#' @param var Variable(s) in which to convert data-points to numbers.
#'
#' @returns A data-frame.
#'
#' @importFrom dplyr across mutate
#'
#' @export

number <- function(data,
                   var) {
  
  dplyr::mutate(.data = data,
                dplyr::across(.cols = {{ var }},
                              .fns = as.numeric,
                              .names = "{.col}"),
                .keep = "all")
  
}