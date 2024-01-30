#' @title Export data-frames as Feather files
#'
#' @description
#' Export data-frames as Feather files.
#'
#' @usage
#' export_feather(x, dirname, rpath)
#'
#' @param x A data-frame.
#' @param dirname A directory above 1) the Feather file and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the Feather file.
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

  path <- zhaoy::path(basename = dirname,
                      rpath)

  feather::write_feather(x = x,
                         path = path)

}