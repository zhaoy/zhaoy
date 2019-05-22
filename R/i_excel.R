#' Import Microsoft Excel files
#'
#' @description
#' Import Microsoft Excel files.
#'
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' i_excel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder a folder above 1) the Excel file and 2) the R file.
#' @param path relative to \code{folder}, path to the Excel file.
#' @param sheet optional: a sheet to import.
#' @param range optional: a cell range to import.
#'
#' @return
#' A data-frame.
#'
#' @importFrom readxl read_excel
#'
#' @export

i_excel <- function(folder,
                    path,
                    sheet = NULL,
                    range = NULL) {

  path <- zhaoy::file_path(dirname = folder,
                           rpath = path)

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
                          progress = FALSE,
                          .name_repair = "universal")

  zhaoy::i_df(x = x)

}