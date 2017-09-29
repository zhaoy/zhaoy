#' Open a customized .R file
#'
#' @description
#' Open a customized .R file in a text editor.
#'
#' @usage
#' new_r()
#'
#' @seealso \code{\link{new_rmd}}
#'
#' @importFrom utils file.edit
#'
#' @export

new_r <- function() {

  options(editor = "internal")

  customized_r <- system.file("customized.R",
                              package = "zhaoy",
                              mustWork = TRUE)

  utils::file.edit(customized_r,
                   title = "customized.R",
                   editor = getOption(x = "editor",
                                      default = "internal"))

}