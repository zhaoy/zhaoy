#' @title 
#' Convert raw data to analyzable data via left joins
#'
#' @description
#' Left-join a raw data-frame and a look-up data-frame.
#'
#' @usage
#' left_convert_helper(raw_df, raw_var, lu_df, lu_category)
#'
#' @param raw_df Raw data-frame.
#' @param raw_var Raw data-frame's variable(s) to convert, data in the variable(s) must be character vector(s) while using this function.
#' @param lu_df Look-up data-frame.
#' @param lu_category Look-up data-frame's category.
#'
#' @return
#' A character vector.
#'
#' @importFrom dplyr filter join_by left_join pull rename select
#' 
#' @noRd

left_convert_helper <- function(raw_df,
                                raw_var,
                                lu_df,
                                lu_category) {
  
  lu_df <- dplyr::filter(.data = lu_df,
                         lu_category == {{ lu_category }})
  
  raw_df <- dplyr::select(.data = raw_df,
                          {{ raw_var }})
  
  var_name <- names(x = raw_df)
  
  raw_df <- dplyr::rename(.data = raw_df,
                          raw = {{ raw_var }})
  
  raw_df <- dplyr::left_join(x = raw_df,
                             y = lu_df,
                             by = dplyr::join_by(raw == raw),
                             keep = NULL,
                             na_matches = "never",
                             multiple = "all",
                             unmatched = "drop",
                             relationship = "many-to-one")
  
  raw_df$raw[! is.na(x = raw_df$analyzable)] <- raw_df$analyzable[! is.na(x = raw_df$analyzable)]
  
  raw_df$raw[! is.na(x = raw_df$analyzable) &
             raw_df$analyzable == "convert to missing"] <- NA
  
  names(x = raw_df)[! is.na(x = names(x = raw_df)) &
                    names(x = raw_df) == "raw"] <- var_name
  
  dplyr::pull(.data = raw_df,
              var = {{ raw_var }})
  
}