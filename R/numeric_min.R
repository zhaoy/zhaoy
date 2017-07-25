numeric_min <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    min <- min(x = x,
               na.rm = TRUE)

  } else {

    min <- NA

  }

  return(value = min)

}