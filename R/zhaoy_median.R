#' @import stats

zhaoy_median <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    x <- stats::median(x = x,
                       na.rm = TRUE)

  } else {

    x <- NA

  }

  return(value = x)

}