#' reg_matches
#'
#' reg_matches
#'
#' reg_matches
#'
#' @param x something
#' @param m something else
#'
#' @return reg_matches
#'
#' @examples
#'
#' reg_matches(x = "hello", m = reg_expr(x = "hello", pattern = "HELLO", ignore.case = TRUE))
#'
#' @export

reg_matches <- function(x,
                        m) {

    regmatches(x = x,
               m = m,
               invert = FALSE)

}