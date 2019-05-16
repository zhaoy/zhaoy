#' Read xls and xlsx files
#'
#' @description
#' Read xls and xlsx files. Translate upper-case alphabetic characters to lower-case. 
#'
#' @usage
#' import_excel(dirname, rpath, sheet = NULL, range = NULL)
#'
#' @param dirname a directory above both 1) the xls / xlsx file and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the xls / xlsx file.
#' @param sheet (optional) sheet to read.
#' @param range (optional) a cell range to read from.
#'
#' @return
#' A tibble.
#' 
#' @seealso
#' \code{\link{export_excel}}
#'
#' @importFrom readxl read_excel
#'
#' @export

import_excel <- function(dirname,
                         rpath,
                         sheet = NULL,
                         range = NULL) {

  path <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)

  x <- readxl::read_excel(path = path,
                          sheet = sheet,
                          range = range,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          trim_ws = TRUE,
                          skip = 0,
                          n_max = Inf,
                          guess_max = 10000,
                          progress = TRUE,
                          .name_repair = "universal")

  zhaoy::lc_df(x = x)

}