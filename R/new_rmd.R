#' Open a customized .Rmd file
#'
#' @description
#' Opens a customized .Rmd file in the environment's text editor.
#'
#' @usage
#' new_rmd()
#'
#' @seealso \code{\link{new_r}}
#' 
#' @importFrom utils file.edit
#'
#' @export

new_rmd <- function() {

  options(editor = "internal")

  customized_rmd <- system.file("customized.Rmd",
                                package = "zhaoy",
                                mustWork = TRUE)

  utils::file.edit(customized_rmd,
                   title = "customized.Rmd",
                   editor = getOption(x = "editor",
                                      default = "internal"))

}