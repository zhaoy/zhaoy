lower_case_col_value <- function(x) {

  if (is.character(x = x) == TRUE) {

    x <- tolower(x = x)

  }

  return(value = x)

}