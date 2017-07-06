#' Read xls and xlsx files.
#'
#' @description
#' Execute readxl::read_excel with assumptions about the values of some arguments.
#'
#' @details
#' \code{col_names}: Use the first row as column names.
#'
#' \code{col_types}: Guess all column types from the spread-sheet.
#'
#' \code{na}: Treat blank cells as missing data.
#'
#' \code{trim_ws}: Trim leading and trailing whitespace.
#'
#' \code{skip}: The minimum number of rows to skip before reading anything, is 0.
#'
#' \code{n_max}: The maximum number of data rows to read is infinite.
#'
#' Convert column names to lower-case.
#'
#' @param criterion \code{criterion} for \code{\link{find_root}}.
#' @param path Path to the xls / xlsx file, excluding the root directory and first path separator.
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#' @param guess_max Maximum number of data rows to use for guessing column types.
#'
#' @import readxl rprojroot
#'
#' @return
#' A tibble.
#'
#' @seealso \code{\link{import_tsv}}
#'
#' @export
#'
#' @examples
#' ds <- readxl_example(path = "datasets.xlsx")
#' dir <- dirname(path = ds)
#' path <- basename(path = ds)
#' setwd(dir = dir)
#' import_excel(criterion = path, path = path, sheet = "iris", range = NULL, guess_max = 10)

import_excel <- function(criterion,
                         path,
                         sheet,
                         range,
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

    return(value = x)

}