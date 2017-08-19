zhaoy_median <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    x <- median(x = x,
                na.rm = TRUE)

  } else {

    x <- NA

  }

  return(value = x)

}