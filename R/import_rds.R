#' @title Import rds files
#'
#' @description
#' Import rds files.
#'
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' import_rds(dirname, rpath)
#'
#' @param dirname A directory above 1) the rds file and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the rds file.
#'
#' @return
#' A tibble.
#'
#' @seealso
#' \code{\link{export_rds}}
#'
#' @importFrom readr read_rds
#'
#' @export

import_rds <- function(dirname,
                       rpath) {

  path <- zhaoy::path(basename = dirname,
                      rpath)

  x <- readr::read_rds(file = path)

  x <- zhaoy::lc_df(x = x)

  zhaoy::squish_df(x = x)

}