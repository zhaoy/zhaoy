zhaoy_mean <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    mean(x = x,
         trim = 0,
         na.rm = TRUE)

  } else {

    return(value = "NOT num")

  }

}