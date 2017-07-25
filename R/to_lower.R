to_lower <- function(x) {

  if (is.character(x = x) == TRUE) {

    x <- tolower(x = x)

  } else {

    x <- x

  }

  return(value = x)

}