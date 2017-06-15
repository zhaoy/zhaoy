#' Import xls and xlsx files
#'
#' Executes readxl::read_excel with assumptions about the values of some arguments.
#'
#' Use the first row as column names.
#'
#' Guess all column types from the spread-sheet.
#'
#' Treat only blank cells as missing data.
#'
#' Trim leading and trailing white-space.
#'
#' The minimum number of rows to skip before reading anything, is zero.
#'
#' The maximum number of data rows to read is infinite.
#'
#' @param criterion Root path criterion.
#' @param file xls / xlsx file.
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#' @param guess_max Maximum number of rows to use for guessing column types.
#'
#' @import readxl rprojroot
#'
#' @export
#'
#' @examples
#' iris <- "datasets.xlsx"
#' datasets <- readxl_example("datasets.xlsx")
#' datasets <- substring(text = datasets, first = 1, last = 49)
#' setwd(dir = datasets)
#' import_excel(criterion = iris, file = iris, sheet = "iris", range = NULL, guess_max = 10)

import_excel <- function(criterion,
                         file,
                         sheet,
                         range,
                         guess_max) {

    root_path <- find_root(criterion = criterion,
                           path = ".")

    import_path <- file.path(root_path,
                             file,
                             fsep = "/")

    x <- read_excel(path = import_path,
                    sheet = sheet,
                    range = range,
                    col_names = TRUE,
                    col_types = NULL,
                    na = "",
                    trim_ws = TRUE,
                    skip = 0,
                    n_max = Inf,
                    guess_max = guess_max)

    return(value = x)

}