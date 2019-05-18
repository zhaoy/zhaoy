#' Build absolute paths to files or folders
#'
#' @description
#' Build absolute paths to files or folders.
#'
#' @usage
#' file_path(dirname, rpath)
#'
#' @param dirname a directory above 1) the file / folder and 2) the R file.
#' @param rpath relative to \code{dirname}, path to the file / folder.
#'
#' @return
#' A character vector.
#'
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

file_path <- function(dirname,
                      rpath) {

  criterion <- rprojroot::has_dirname(dirname = dirname)

  root <- rprojroot::find_root(criterion = criterion,
                               path = ".")

  file.path(root,
            rpath,
            fsep = "/")

}