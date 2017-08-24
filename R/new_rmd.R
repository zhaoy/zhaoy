#' new_rmd
#'
#' @description
#' new_rmd
#'
#' @usage
#' new_rmd()
#'
#' @return A data-frame.
#'
#' @seealso \code{\link{new_r}}
#'
#' @export

new_rmd <- function() {

  options(editor = "internal")

  template_rmd <- system.file("template.Rmd",
                              package = "zhaoy",
                              mustWork = TRUE)

  file.edit(template_rmd,
            title = "template.Rmd",
            editor = getOption(x = "editor",
                               default = "internal"))

}