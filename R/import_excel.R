#' Read xls and xlsx files.
#'
#' @description
#' Execute readxl::\code{\link{read_excel}} with pre-set values in some arguments.
#' Convert column names and categorical data to lower-case.
#'
#' @usage
#' import_excel(folder, file, sheet = NULL, range = NULL, guess_max)
#'
#' @param folder Folder immediately above the xls / xlsx file.
#' @param file File name.
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#' @param guess_max Maximum number of data rows to use for guessing column types.
#'
#' @details
#' The readxl::\code{\link{read_excel}} pre-set argument values are
#'
#' \code{col_names = TRUE}: Use the first row as column names.
#'
#' \code{col_types = NULL}: Guess all column types from the spread-sheet.
#'
#' \code{na = ""}: Treat blank cells as missing data.
#'
#' \code{trim_ws = TRUE}: Trim leading and trailing white-space.
#'
#' \code{skip = 0}: The minimum number of rows to skip before reading anything is 0.
#'
#' \code{n_max = Inf}: The maximum number of data rows to read is \code{\link{Inf}}.
#'
#' @return A table.
#'
#' @import purrr readxl rprojroot
#'
#' @export

import_excel <- function(folder,
                         file,
                         sheet = NULL,
                         range = NULL,
                         guess_max) {

  root_path <- rprojroot::find_root(criterion = has_dirname(dirname = folder),
                                    path = ".")

  import_path <- file.path(root_path,
                           file,
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

  x %>%
    map(.f = zhaoy_tolower) %>%
    as.data.frame(row.names = NULL,
                  stringsAsFactors = FALSE,
                  cut.names = TRUE,
                  fix.empty.names = TRUE)

}