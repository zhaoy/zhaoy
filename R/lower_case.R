#' @title Convert data-points to lower-case
#'
#' @description Convert data-points to lower-case.
#'
#' @usage lower_case(data, var)
#'
#' @param data Data-frame.
#' @param var Variable(s) in which to convert data-points to lower-case.
#'
#' @returns A data-frame.
#'
#' @importFrom dplyr across mutate
#' @importFrom stringr str_to_lower
#'
#' @export

lower_case <- function(data,
                       var) {
  
  dplyr::mutate(.data = data,
                dplyr::across(.cols = {{ var }},
                              .fns = function(x)
                                     stringr::str_to_lower(string = x,
                                                           locale = "en"),
                              .names = "{.col}"),
                .keep = "all")
  
}