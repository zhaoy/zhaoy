#' @title Import Microsoft Excel files
#'
#' @description
#' Import Microsoft Excel files.
#'
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' import_excel(dirname, rpath, sheet = NULL, range = NULL)
#'
#' @param dirname A directory above 1) the Excel file and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the Excel file.
#' @param sheet Optional: a sheet to import.
#' @param range Optional: a cell range to import.
#'
#' @return
#' A tibble.
#'
#' @seealso
#' \code{\link{export_excel}}
#'
#' @importFrom readxl read_excel
#' @importFrom vctrs vec_as_names
#'
#' @export

import_excel <- function(dirname,
                         rpath,
                         sheet = NULL,
                         range = NULL) {

  path <- zhaoy::path(basename = dirname,
                      rpath)

  x <- readxl::read_excel(path = path,
                          sheet = sheet,
                          range = range,
                          col_names = TRUE,
                          col_types = "text",
                          na = "",
                          trim_ws = TRUE,
                          skip = 0,
                          n_max = Inf,
                          guess_max = 1000,
                          progress = FALSE,
                          .name_repair = "minimal")

  names(x = x) <- vctrs::vec_as_names(names = names(x = x),
                                      repair = "universal",
                                      quiet = TRUE)

  x <- zhaoy::lc_df(x = x)

  zhaoy::squish_df(x = x)

}