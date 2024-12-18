#' @title
#' Convert relative paths to absolute paths
#'
#' @description
#' If the working directory is below the root directory, convert a relative path to an absolute path.
#'
#' @usage
#' path(basename, ...)
#'
#' @param basename Root directory.
#' @param ... Relative to \code{basename}, path to the file or folder.
#'
#' @return
#' A vector.
#'
#' @importFrom rprojroot find_root_file has_basename
#'
#' @export

path <- function(basename,
                 ...) {
  
  criterion <- rprojroot::has_basename(basename = basename)
  
  path <- rprojroot::find_root_file(...,
                                    criterion = criterion,
                                    path = ".")
  
}