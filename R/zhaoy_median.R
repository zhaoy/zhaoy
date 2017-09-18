#' @import stats

zhaoy_median <- function(x) {

  if (is.numeric(x = x) == TRUE) {

    stats::median(x = x,
                  na.rm = TRUE)

  } else {

    return(value = "NOT num")

  }

}