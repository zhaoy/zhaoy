#' Import Feather files
#'
#' @description
#' Import Feather files.
#'
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' i_feather(folder, path)
#'
#' @param folder a folder above 1) the Feather file and 2) the R file.
#' @param path relative to \code{folder}, path to the Feather file.
#'
#' @return
#' A data-frame.
#'
#' @importFrom feather read_feather
#'
#' @export

i_feather <- function(folder,
                      path) {

  path <- zhaoy::file_path(dirname = folder,
                           rpath = path)

  x <- feather::read_feather(path = path)

  zhaoy::i_df(x = x)

}