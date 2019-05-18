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
#' @export

import_rds <- function(dirname,
                       rpath) {

  file <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)

  x <- readRDS(file = file)

  zhaoy::lc_df(x = x)

}