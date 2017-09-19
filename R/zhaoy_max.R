zhaoy_max <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    max(x = x,
        na.rm = TRUE)

  } else {

    return(value = NA)

  }

}
