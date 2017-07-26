#' new_rmd
#'
#' @description
#' new_rmd
#'
#' @usage
#' \code{new_rmd()}
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
                              lib.loc = NULL,
                              mustWork = TRUE)

  file.edit(template_rmd)

}