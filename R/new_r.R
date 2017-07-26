#' new_r
#'
#' @description
#' new_r
#'
#' @usage
#' \code{new_r()}
#'
#' @return A data-frame.
#'
#' @seealso \code{\link{new_rmd}}
#'
#' @export

new_r <- function() {

  options(editor = "internal")

  template_r <- system.file("template.R",
                            package = "zhaoy",
                            lib.loc = NULL,
                            mustWork = TRUE)

  file.edit(template_r)

}