#' Read rds files
#'
#' @description
#' Read rds files. Translate upper-case alphabetic characters to lower-case.
#'
#' @usage
#' import_rds(dirname, rpath)
#'
#' @param dirname a directory above both 1) the rds file and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the rds file.
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