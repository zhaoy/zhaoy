#' Read xls and xlsx files
#'
#' Executes readxl::read_excel with assumptions about the values of some arguments.
#'
#' Use the first row as column names.
#'
#' Guess all column types from the spread-sheet.
#'
#' Treats blank cells as missing data.
#'
#' Trim leading and trailing white-space.
#'
#' The minimum number of rows to skip before reading anything, is zero.
#'
#' The maximum number of data rows to read is infinite.
#'
#' @param criterion A criterion
#' @param file xls / xlsx file
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#' @param guess_max Maximum number of data rows to use for guessing column types.
#'
#' @import readxl rprojroot
#'
#' @export
#'
#' @examples
#' ds <- "datasets.xlsx"
#' ds_dir <- readxl_example(path = "datasets.xlsx")
#' ds_dir <- substring(text = ds_dir, first = 1, last = 49)
#' setwd(dir = ds_dir)
#' import_excel(criterion = ds, file = ds, sheet = "iris", range = NULL, guess_max = 10)

import_excel <- function(criterion,
                         file,
                         sheet,
                         range,
                         guess_max) {

    root_path <- rprojroot::find_root(criterion = criterion,
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

    return(value = x)

}