#' Write Feather files
#'
#' @description
#' Write Feather files.
#'
#' @usage
#' export_feather(x, dirname, rpath)
#'
#' @param x a data-frame to write to disk.
#' @param dirname a directory above both 1) the future Feather file and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the future Feather file.
#'
#' @seealso
#' \code{\link{import_feather}}
#'
#' @importFrom feather write_feather
#'
#' @export

export_feather <- function(x,
                           dirname,
                           rpath) {

  path <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)

  feather::write_feather(x = x,
                         path = path)

}