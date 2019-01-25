#' Read Feather files.
#'
#' @description
#' Read Feather files.
#'
#' @usage
#' i_feather(folder, path)
#'
#' @param folder any folder above both 1) the Feather file and 2) the R file.
#' @param path relative to \code{folder}, path to Feather file.
#'
#' @return
#' A data-frame.
#'
#' @importFrom feather read_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

i_feather <- function(folder,
                      path) {

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  path <- file.path(root,
                    path,
                    fsep = "/")

  x <- feather::read_feather(path = path)

  zhaoy::i_df(x = x)

}