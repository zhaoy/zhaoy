#' Open a .Rmd template.
#'
#' @description
#' Open a .Rmd template in a text editor.
#'
#' @usage
#' new_rmd()
#'
#' @seealso
#' \code{\link{new_r}}
#'
#' @importFrom utils file.edit
#'
#' @export

new_rmd <- function() {

  options(editor = "internal")

  template_rmd <- system.file("template.Rmd",
                              package = "zhaoy",
                              mustWork = TRUE)

  utils::file.edit(template_rmd,
                   title = "template.Rmd",
                   editor = getOption(x = "editor",
                                      default = "internal"))

}