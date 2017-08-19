zhaoy_min <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    x <- min(x = x,
             na.rm = TRUE)

  } else {

    x <- NA

  }

  return(value = x)

}