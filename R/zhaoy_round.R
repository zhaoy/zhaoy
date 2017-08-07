zhaoy_round <- function(x) {

  if (is.numeric(x = x) == TRUE &
      is.integer(x = x) == FALSE) {

    x <- round(x = x,
               digits = 2)

  }

  return(value = x)

}