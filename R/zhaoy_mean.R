zhaoy_mean <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    x <- mean(x = x,
              trim = 0,
              na.rm = TRUE)

  } else {

    x <- NA

  }

  return(value = x)

}