#' @title Import rds files
#'
#' @description Import .rds files.
#'
#' @usage import_rds(dirname, rpath)
#'
#' @param dirname A directory above both the .rds file and .R file.
#' @param rpath Relative to \code{dirname}, path to the .rds file.
#'
#' @returns A data-frame.
#'
#' @importFrom readr read_rds
#'
#' @export

import_rds <- function(dirname,
                       rpath) {

  file <- gwep::path(basename = dirname,
                     rpath)

  readr::read_rds(file = file)

}