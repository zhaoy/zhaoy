#' Export data-frames as rds files.
#'
#' @description
#' Export data-frames as rds files.
#'
#' @usage
#' export_rds(x, dirname, rpath)
#'
#' @param x a data-frame.
#' @param dirname a directory above 1) the rds file and 2) the R file.
#' @param rpath relative to \code{dirname}, path to the rds file.
#'
#' @seealso
#' \code{\link{import_rds}}
#'
#' @importFrom readr write_rds
#'
#' @export

export_rds <- function(x,
                       dirname,
                       rpath) {

  path <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)

  readr::write_rds(x = x,
                   path = path)

}