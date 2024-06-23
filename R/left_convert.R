#' @title 
#' Convert data by left-joining data-frames and look-up tables
#'
#' @description
#' Convert data by left-joining data-frames and look-up tables.
#'
#' @usage
#' left_convert(x_df, x_var, y_df, category)
#'
#' @param x_df Left-hand-side data-frame.
#' @param x_var Left-hand-side variable(s).
#' @param y_df Right-hand-side data-frame.
#' @param category Right-hand-side data-frame's category.
#'
#' @return
#' A data-frame.
#'
#' @importFrom dplyr across mutate
#' 
#' @export

left_convert <- function(x_df,
                         x_var,
                         y_df,
                         category) {
  
  dplyr::mutate(.data = x_df,
                dplyr::across(.cols = {{ x_var }},
                              .fns = function(x)
                              left_convert_helper(x_df = {{ x_df }},
                                                  x_var = x,
                                                  y_df = {{ y_df }},
                                                  category = {{ category }}),
                              .names = "{.col}"),
                .keep = "all")
  
}