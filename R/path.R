#' @title Convert relative file-paths to absolute
#'
#' @description
#' Convert relative file-paths to absolute.
#'
#' @usage
#' path(dirname, rpath)
#'
#' @param dirname A directory above 1) the file / folder and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the file / folder.
#'
#' @return
#' A character vector.
#'
#' @importFrom fs path
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

path <- function(dirname,
                 rpath) {

  criterion <- rprojroot::has_dirname(dirname = dirname)

  root <- rprojroot::find_root(criterion = criterion,
                               path = ".")

  fs::path(root,
           rpath)

}