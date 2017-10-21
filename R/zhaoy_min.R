zhaoy_min <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    min(x = x,
        na.rm = TRUE)

  } else if (is.numeric(x = x) == FALSE) {

    return(value = NA)

  }

}