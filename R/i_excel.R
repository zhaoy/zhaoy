#' Read xls and xlsx files
#'
#' @description
#' Read xls and xlsx files. Translate upper-case alphabetic characters to lower-case.
#'
#' @usage
#' i_excel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder a folder above both 1) the xls / xlsx file and 2) the R file.
#' @param path relative to \code{folder}, the path to the xls / xlsx file.
#' @param sheet (optional) sheet to read.
#' @param range (optional) a cell range to read from.
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
                          .name_repair = "universal")

  zhaoy::i_df(x = x)

}