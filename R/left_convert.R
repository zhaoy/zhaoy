#' @title Convert raw data to analyzable data via left joins
#'
#' @description Left-join a raw data-frame and a look-up data-frame.
#'
#' @usage left_convert(raw_df, raw_var, lu_df, lu_category)
#'
#' @inheritParams left_convert_helper
#'
#' @return A data-frame.
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