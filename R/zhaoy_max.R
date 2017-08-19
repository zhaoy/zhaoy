zhaoy_max <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    x <- max(x = x,
             na.rm = TRUE)

  } else {

    x <- NA

  }

  return(value = x)

}