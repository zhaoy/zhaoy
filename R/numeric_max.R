numeric_max <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    max <- max(x = x,
               na.rm = TRUE)

  } else {

    max <- NA

  }

  return(value = max)

}