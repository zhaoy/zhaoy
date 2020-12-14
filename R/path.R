#' @title Convert relative file-paths to absolute file-paths
#'
#' @description
#' Convert relative file-paths to absolute file-paths.
#'
#' @usage
#' path(basename, rpath)
#'
#' @param basename A folder above the 1) target file / folder and 2) R-code file.
#' @param rpath Relative to \code{basename}, path to the target file / folder.
#'
#' @return
#' A character vector.
#'
#' @importFrom rprojroot find_root_file has_basename
#'
#' @export

path <- function(basename,
                 rpath) {

  criterion <- rprojroot::has_basename(basename = basename)

  rprojroot::find_root_file(rpath,
                            criterion = criterion,
                            path = ".")

}