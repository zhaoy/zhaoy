#' Read xls and xlsx files.
#'
#' @description
#' Execute readxl::\code{\link{read_excel}} with pre-set values in some arguments.
#' Convert column names and categorical data to lower-case.
#'
#' @usage
#' \code{import_excel(criterion, path, sheet = NULL, range = NULL, guess_max = 10)}
#'
#' @param criterion \code{criterion} argument of rprojroot::\code{\link{find_root}}.
#' @param path Path to the xls / xlsx file, excluding the root directory.
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
#' @return
#' A data-frame.
#'
#' @seealso \code{\link{import_tsv}}
#'
#' @import readxl rprojroot
#'
#' @export
#'
#' @examples
#' library(package = readxl)
#' datasets <- readxl_example(path = "datasets.xlsx")
#' dir <- dirname(path = datasets)
#' base <- basename(path = datasets)
#' setwd(dir = dir)
#' import_excel(criterion = base, path = base, guess_max = 10)

import_excel <- function(criterion,
                         path,
                         sheet = NULL,
                         range = NULL,
                         guess_max) {

  root_path <- rprojroot::find_root(criterion = criterion,
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

  x <- lapply(X = x,
              FUN = to_lower)

  x <- data.frame(x = x,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  return(value = x)

}