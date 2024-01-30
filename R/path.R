#' @title Convert relative paths to absolute paths
#'
#' @description
#' If the working directory is the target folder or below, convert relative paths to absolute paths.
#'
#' @usage
#' path(basename, ...)
#'
#' @param basename The target folder.
#' @param ... Relative to \code{basename}, path to the target file.
#'
#' @return
#' A character vector.
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

  as.character(x = path)

}