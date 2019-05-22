#' Import rds files
#'
#' @description
#' Import rds files.
#'
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' import_rds(dirname, rpath)
#'
#' @param dirname a directory above 1) the rds file and 2) the R file.
#' @param rpath relative to \code{dirname}, path to the rds file.
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

  path <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)

  x <- readr::read_rds(path = path)

  zhaoy::lc_df(x = x)

}