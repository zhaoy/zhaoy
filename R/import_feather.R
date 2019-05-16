#' Read Feather files
#'
#' @description
#' Read Feather files. Translate upper-case alphabetic characters to lower-case.
#'
#' @usage
#' import_feather(dirname, rpath)
#'
#' @param dirname a directory above both 1) the Feather file and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the Feather file.
#'
#' @return
#' A tibble.
#'
#' @seealso
#' \code{\link{export_feather}}
#'
#' @importFrom feather read_feather
#'
#' @export

import_feather <- function(dirname,
                           rpath) {

  path <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)

  x <- feather::read_feather(path = path)

  zhaoy::lc_df(x = x)

}