#' Read Feather files.
#'
#' @description
#' Combines \code{feather::read_feather} and \code{rprojroot::find_root}.
#'
#' Converts upper-case column names and categorical data to lower-case.
#'
#' @usage
#' import_feather(folder, path)
#'
#' @param folder Any folder above both 1) the Feather file and 2) the code file.
#' @param path Relative to \code{folder}, path to the Feather file.
#'
#' @return
#' A base-R data-frame.
#'
#' @seealso
#' \code{\link{import_df}}
#' \code{\link{import_excel}}
#'
#' @importFrom feather read_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_feather <- function(folder,
                           path) {

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  full_path <- file.path(root_path,
                         path,
                         fsep = "/")

  x <- feather::read_feather(path = full_path)

  zhaoy::import_df(x = x)

}