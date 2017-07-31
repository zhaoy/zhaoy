zhaoy_median <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    x <- median(x = x,
                na.rm = TRUE)

  }

  if (is.numeric(x = x) == FALSE) {

    x <- NA

  }

  return(value = x)

}