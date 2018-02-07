#' Read xls and xlsx files.
#'
#' @description
#' Read xls and xlsx files.
#'
#' Convert upper-case variable names and categorical data to lower-case.
#'
#' @usage
#' import_excel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder any folder above both 1) the xls / xlsx file and 2) the R file.
#' @param path relative to \code{folder}, path to the xls / xlsx file.
#' @param sheet sheet to read.
#' @param range a cell range to read from.
#'
#' @return
#' A base-R data-frame.
#'
#' @importFrom readxl read_excel
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_excel <- function(folder,
                         path,
                         sheet = NULL,
                         range = NULL) {

  # is.character() is necessary because
  # nzchar() and grepl() can return TRUE / FALSE
  # for non-character inputs.

  stopifnot(is.character(x = folder),
            nzchar(x = folder,
                   keepNA = TRUE),
            is.character(x = path),
            grepl(pattern = "[.xls]|[.xlsx]",
                  x = path,
                  ignore.case = TRUE))

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  import_path <- file.path(root,
                           path,
                           fsep = "/")

  x <- readxl::read_excel(path = import_path,
                          sheet = sheet,
                          range = range,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          trim_ws = TRUE,
                          skip = 0,
                          n_max = Inf,
                          guess_max = 100000)

  zhaoy::import_df(x = x)

}