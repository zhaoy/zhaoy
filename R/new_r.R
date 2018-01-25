#' Open a .R template.
#'
#' @description
#' Open a .R template in a text editor.
#'
#' @usage
#' new_r()
#'
#' @seealso
#' \code{\link{new_rmd}}
#'
#' @importFrom utils file.edit
#'
#' @export

new_r <- function() {

  options(editor = "internal")

  template_r <- system.file("template.R",
                            package = "zhaoy",
                            mustWork = TRUE)

  utils::file.edit(template_r,
                   title = "template.R",
                   editor = getOption(x = "editor",
                                      default = "internal"))

}