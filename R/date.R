#' @title Convert data-points to yyyy-mm-dd
#'
#' @description Convert data-points to yyyy-mm-dd.
#'
#' @usage date(data, var, format)
#'
#' @param data Data-frame.
#' @param var Variable(s) in which to convert data-points to yyyy-mm-dd.
#' @param format Date format(s) from which to convert to yyyy-mm-dd.
#'
#' @returns A data-frame.
#'
#' @importFrom dplyr across mutate
#' @importFrom lubridate as_date
#'
#' @export

date <- function(data,
                 var,
                 format) {
  
  dplyr::mutate(.data = data,
                dplyr::across(.cols = {{ var }},
                              .fns = function(x)
                                     lubridate::as_date(x = x,
                                                        format = format),
                              .names = "{.col}"),
                .keep = "all")
  
}