left_convert_2 <- function(category,
                           df,
                           var) {
  
  mutate(.data = df,
         dplyr::across(.cols = {{ var }},
                       .fns = function(x)
                              temp_internal(category = category,
                                            df = df,
                                            var = x),
                       .names = "{.col}"),
         .keep = "all")
  
}