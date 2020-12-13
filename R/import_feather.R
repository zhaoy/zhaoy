#' @title Import Feather files
#'
#' @description
#' Import Feather files.
#'
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' import_feather(dirname, rpath)
#'
#' @param dirname A directory above 1) the Feather file and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the Feather file.
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

  path <- zhaoy::path(basename = dirname,
                      rpath = rpath)

  x <- feather::read_feather(path = path)

  zhaoy::lc_df(x = x)

}