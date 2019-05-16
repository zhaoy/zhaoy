#' Construct path to file or folder
#'
#' @description
#' Construct path to file or folder.
#'
#' @usage
#' file_path(dirname, rpath)
#'
#' @param dirname a directory above both 1) the file / folder and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the file / folder.
#'
#' @return
#' A character vector.
#'
#' @importFrom rprojroot find_root has_dirname
#'
#' @export
 
file_path <- function(dirname,
                      rpath) {

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = dirname),
                               path = ".")

  file.path(root,
            rpath,
            fsep = "/")

}