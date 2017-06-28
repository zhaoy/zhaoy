#' g_sub
#'
#' g_sub
#'
#' g_sub
#'
#' @param x something
#' @param pattern something else
#' @param replacement something something
#' @param ignore.case something something else
#'
#' @return g_sub
#'
#' @examples
#'
#' g_sub(x = "hello", pattern = "HELLO", replacement = "", ignore.case = TRUE)
#'
#' @export

g_sub <- function(x,
                  pattern,
                  replacement,
                  ignore.case) {

    gsub(pattern = pattern,
         x = x,
         ignore.case = ignore.case,
         perl = TRUE,
         fixed = FALSE,
         useBytes = FALSE,
         replacement = replacement)

}