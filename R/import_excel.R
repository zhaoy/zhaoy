#' Read xls and xlsx files.
#'
#' @description
#' Executes readxl::\code{\link{read_excel}} with pre-set values in some arguments.
#' Converts column names and categorical data to lower-case.
#'
#' @usage
#' import_excel(folder, path, sheet = NULL, range = NULL, guess_max)
#'
#' @param folder Folder above the xls / xlsx file.
#' @param path Path to the file, excluding \code{folder}.
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#' @param guess_max Maximum number of data rows to use for guessing column types.
#'
#' @details
#' The pre-set values in some arguments of readxl::\code{\link{read_excel}}, are:
#'
#' \code{col_names = TRUE}: Use the first row as column names.
#'
#' \code{col_types = NULL}: Guess all column types from the spread-sheet.
#'
#' \code{na = ""}: Treat blank cells as missing data.
#'
#' \code{trim_ws = TRUE}: Trim leading and trailing white-space.
#'
#' \code{skip = 0}: Skip a minimum of 0 rows before reading anything.
#'
#' \code{n_max = Inf}: Read a maximum of \code{\link{Inf}} rows.
#'
#' @return A \code{\link{data.frame}}.
#'
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom rprojroot find_root has_dirname
#' @import feather
#'
#' @export

import_excel <- function(folder,
                         path,
                         sheet = NULL,
                         range = NULL,
                         guess_max) {

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  import_path <- file.path(root_path,
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
                          guess_max = guess_max)

  names(x = x) <- tolower(x = names(x = x))

  x <- x %>%
    purrr::map(.f = zhaoy_tolower) %>%
    as.data.frame(row.names = NULL,
                  stringsAsFactors = FALSE,
                  cut.names = TRUE,
                  fix.empty.names = TRUE)

}