numeric_median <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    median <- median(x = x,
                     na.rm = TRUE)

  } else {

    median <- NA

  }

  return(value = median)

}