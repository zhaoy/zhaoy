#' Write Feather files.
#'
#' @description
#' Write Feather files.
#'
#' @usage
#' export_feather(x, folder, path)
#'
#' @param x a data-frame to write to disk.
#' @param folder any folder above the R file.
#' @param path relative to \code{folder}, path to Feather file.
#'
#' @seealso
#' \code{\link{import_feather}}
#'
#' @importFrom feather write_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

export_feather <- function(x,
                           folder,
                           path) {

  stopifnot(is.data.frame(x = x) == TRUE)

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  path <- file.path(root,
                    path,
                    fsep = "/")

  feather::write_feather(x,
                         path = path)

}