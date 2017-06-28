#' grep_l
#'
#' grep_l
#'
#' grep_l
#'
#' @param x something
#' @param pattern something else
#' @param ignore.case something something else
#'
#' @return grep_l.
#'
#' @examples
#'
#' grep_l(x = "hello", pattern = "HELLO", ignore.case = TRUE)
#'
#' @export

grep_l <- function(x,
                   pattern,
                   ignore.case) {

    x <- grepl(pattern = pattern,
               x = x,
               ignore.case = ignore.case,
               perl = TRUE,
               fixed = FALSE,
               useBytes = FALSE)

    return(value = x)

}