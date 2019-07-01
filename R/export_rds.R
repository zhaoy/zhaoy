#' @title Export data-frames as rds files
#'
#' @description
#' Export data-frames as rds files.
#'
#' @usage
#' export_rds(x, dirname, rpath)
#'
#' @param x A data-frame.
#' @param dirname A directory above 1) the rds file and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the rds file.
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

  path <- zhaoy::path(dirname = dirname,
                      rpath = rpath)

  readr::write_rds(x = x,
                   path = path,
                   compress = "gz")

}