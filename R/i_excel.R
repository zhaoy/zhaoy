#' Read xls and xlsx files.
#'
#' @description
#' Read xls and xlsx files.
#'
#' @usage
#' i_excel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder any folder above both 1) the xls / xlsx file and 2) the R file.
#' @param path relative to \code{folder}, path to the xls / xlsx file.
#' @param sheet sheet to read.
#' @param range a cell range to read from.
#'
#' @return
#' A data-frame.
#'
#' @importFrom readxl read_excel
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

i_excel <- function(folder,
                    path,
                    sheet = NULL,
                    range = NULL) {

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  path <- file.path(root,
                    path,
                    fsep = "/")

  x <- readxl::read_excel(path = path,
                          sheet = sheet,
                          range = range,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          trim_ws = TRUE,
                          skip = 0,
                          n_max = Inf,
                          guess_max = 100000,
                          .name_repair = "universal")

  zhaoy::i_df(x = x)

}