#' Sort unique values of vectors.
#'
#' Sort unique values of vectors, in ascending order beginning with any NAs.
#'
#' Values can be categories, numbers, or strings.
#'
#' @param x A vector.
#'
#' @return
#'
#' @keywords
#'
#' @export
#'
#' @examples
#' sort_unique(x = iris$Sepal.Length)
#' sort_unique(x = mtcars$mpg)

sort_unique <- function(x) {

    x <- unique(x = x)

    x <- sort(x = x,
              decreasing = FALSE,
              na.last = FALSE)

    return(value = x)

}