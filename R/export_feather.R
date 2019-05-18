#' Export data-frames as Feather files.
#'
#' @description
#' Export data-frames as Feather files.
#'
#' @usage
#' export_feather(x, dirname, rpath)
#'
#' @param x a data-frame.
#' @param dirname a directory above 1) the Feather file and 2) the R file.
#' @param rpath relative to \code{dirname}, path to the Feather file.
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