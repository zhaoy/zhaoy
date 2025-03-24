#' @title Export data-frames as .rds files
#'
#' @description Export data-frames as .rds files.
#'
#' @usage export_rds(x, dirname, rpath)
#'
#' @param x A data-frame.
#' @param dirname A directory above both the .rds file and .R file.
#' @param rpath Relative to \code{dirname}, path to the .rds file.
#'
#' @returns \code{export_rds()} returns \code{x} invisibly.
#'
#' @importFrom readr write_rds
#'
#' @export

export_rds <- function(x,
                       dirname,
                       rpath) {

  file <- gwep::path(basename = dirname,
                     rpath)

  readr::write_rds(x = x,
                   file = file,
                   compress = "none")

}