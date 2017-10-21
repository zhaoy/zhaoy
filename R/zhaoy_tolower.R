zhaoy_tolower <- function(x) {

  if (is.character(x = x) == TRUE |
      is.factor(x = x) == TRUE) {

    tolower(x = x)

  } else {

    return(value = x)

  }

}