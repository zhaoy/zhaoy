left_convert_internal <- function(category,
                                  df,
                                  var) {
  
  y_df <- dplyr::filter(.data = convert_data,
                        category == category)
  
  x_df <- select(.data = df,
                 {{ var }})
  
  var_name <- names(x = x_df)
  
  x_df <- dplyr::rename(.data = x_df,
                        raw = {{ var }})
  
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
              var = {{ var }})
  
}