#' Extract and Order Unique Elements of Vectors
#'
#' Order unique elements of vectors or factors into ascending order, beginning with any NAs.
#'
#' @param x an R object with a class or a numeric, complex, character, or logical vector.
#'
#' @export
#'
#' @examples
#' order_unique(x = iris$Sepal.Length)
#' order_unique(x = mtcars$mpg)

order_unique <- function(x) {

    x <- unique(x = x)

    x <- sort(x = x,
              decreasing = FALSE,
              na.last = FALSE)

    return(value = x)

}