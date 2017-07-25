numeric_mean <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    mean <- mean(x = x,
                 na.rm = TRUE)

  } else {

    mean <- NA

  }

  return(value = mean)

}