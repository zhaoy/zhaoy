#' Write rds files
#'
#' @description
#' Write rds files.
#'
#' @usage
#' export_rds(object, dirname, rpath)
#'
#' @param object R object to serialize.
#' @param dirname a directory above both 1) the future rds file and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the future rds file.
#'
#' @seealso
#' \code{\link{import_rds}}
#' 
#' @export

export_rds <- function(object,
                       dirname,
                       rpath) {
  
  file <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)
  
  saveRDS(object = object,
          file = file)
  
}