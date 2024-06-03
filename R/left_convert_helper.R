#' @title 
#' Convert data by left-joining data-frames and look-up tables.
#'
#' @description
#' Convert data by left-joining data-frames and look-up tables.
#'
#' @usage
#' left_convert_helper(x_df, x_var, y_df, category)
#'
#' @param x_df Left-hand-side data-frame.
#' @param x_var Left-hand-side variable.
#' @param y_df Right-hand-side data-frame.
#' @param category Right-hand-side data-frame's category.
#'
#' @return
#' A vector.
#'
#' @importFrom dplyr filter join_by left_join pull rename select

left_convert_helper <- function(x_df,
                                x_var,
                                y_df,
                                category) {
  
  y_df <- dplyr::filter(.data = y_df,
                        category == category)
  
  x_df <- dplyr::select(.data = x_df,
                        {{ x_var }})
  
  var_name <- names(x = x_df)
  
  x_df <- dplyr::rename(.data = x_df,
                        raw = {{ x_var }})
  
  x_df <- dplyr::left_join(x = x_df,
                           y = y_df,
                           by = dplyr::join_by("raw"),
                           keep = NULL,
                           multiple = "all",
                           unmatched = "drop",
                           relationship = "many-to-one")
  
  x_df$raw[! is.na(x = x_df$analyzable)] <- x_df$analyzable[! is.na(x = x_df$analyzable)]
  
  x_df$raw[! is.na(x = x_df$analyzable) &
           x_df$analyzable == "convert to missing"] <- NA
  
  names(x = x_df)[! is.na(x = names(x = x_df)) &
                  names(x = x_df) == "raw"] <- var_name
  
  dplyr::pull(.data = x_df,
              var = {{ x_var }})
  
}