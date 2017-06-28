#' reg_expr
#'
#' reg_expr
#'
#' reg_expr
#'
#' @param x something
#' @param pattern something else
#' @param ignore.case something something else
#'
#' @return reg_expr
#'
#' @examples
#'
#' reg_expr(x = "hello", pattern = "HELLO", ignore.case = TRUE)
#'
#' @export

reg_expr <- function(x,
                     pattern,
                     ignore.case) {

    regexpr(pattern = pattern,
            text = x,
            ignore.case = ignore.case,
            perl = TRUE,
            fixed = FALSE,
            useBytes = FALSE)

}