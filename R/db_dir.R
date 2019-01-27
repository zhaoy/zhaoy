#' Locations of embedded databases.
#'
#' @description
#' Locations of embedded databases.
#'
#' @usage
#' db_dir(folder, path)
#'
#' @param folder any folder above both 1) the database and 2) the R file.
#' @param path relative to \code{folder}, path to the database.
#'
#' @return
#' A length-one character vector.
#'
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

db_dir <- function(folder,
                   path) {

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  file.path(root,
            path,
            fsep = "/")

}