#' Open a customized .Rmd template
#'
#' @description
#' Open a customized .Rmd template in a text editor.
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