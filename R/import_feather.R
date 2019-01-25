#' Read Feather files.
#'
#' @description
#' Read Feather files.
#'
#' @usage
#' import_feather(folder, path)
#'
#' @param folder any folder above both 1) the Feather file and 2) the R file.
#' @param path relative to \code{folder}, path to Feather file.
#'
#' @seealso
#' \code{\link{export_feather}}
#'
#' @importFrom feather read_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_feather <- function(folder,
                           path) {

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  path <- file.path(root,
                    path,
                    fsep = "/")

  x <- feather::read_feather(path = path)

  zhaoy::lc_df(x = x)

}