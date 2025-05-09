#' @title Convert raw data-points to analyzable data-points via left joins
#'
#' @description Left-join a raw data-frame and a look-up data-frame.
#'
#' @usage left_convert(raw_df, raw_var, lu_df, lu_category)
#'
#' @param raw_df Raw data-frame.
#' @param raw_var Raw data-frame's variable(s) to convert. While using this
#'   function, data in the variable(s) must be character vector(s).
#' @param lu_df Look-up data-frame.
#' @param lu_category Look-up data-frame's "category" variable's data-point.
#'
#' @returns A data-frame.
#'
#' @importFrom dplyr across mutate
#'
#' @export

left_convert <- function(raw_df,
                         raw_var,
                         lu_df,
                         lu_category) {
  
  dplyr::mutate(.data = raw_df,
                dplyr::across(.cols = {{ raw_var }},
                              .fns = function(x)
                                     left_convert_helper(raw_df = {{ raw_df }},
                                                         raw_var = x,
                                                         lu_df = {{ lu_df }},
                                                         lu_category = {{ lu_category }}),
                              .names = "{.col}"),
                .keep = "all")
  
}