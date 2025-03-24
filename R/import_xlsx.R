#' @title Import .xlsx files
#'
#' @description Import .xlsx files.
#'
#' @usage import_xlsx(dirname, rpath, sheet = NULL, range = NULL)
#'
#' @param dirname A directory above both the .xlsx file and .R file.
#' @param rpath Relative to \code{dirname}, path to the .xlsx file.
#' @param sheet Optional: a sheet to import.
#' @param range Optional: a cell range to import.
#'
#' @returns A data-frame.
#'
#' @importFrom readxl read_xlsx
#'
#' @export

import_xlsx <- function(dirname,
                        rpath,
                        sheet = NULL,
                        range = NULL) {

  path <- gwep::path(basename = dirname,
                     rpath)

  readxl::read_xlsx(path = path,
                    sheet = sheet,
                    range = range,
                    col_names = TRUE,
                    col_types = "text",
                    na = "",
                    trim_ws = TRUE,
                    skip = 0,
                    n_max = Inf,
                    guess_max = 0,
                    progress = FALSE,
                    .name_repair = "minimal")

}