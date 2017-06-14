#' Import Microsoft Excel files
#'
#' Execute readxl::read_excel with assumptions about the values of some arguments.
#'
#' Range takes precedence over skip, n_max, and sheet. If Range is un-necessary, set it to NULL.
#' First rows are column names.
#' Guess column types from the spread-sheet.
#' Only blank cells represent missing data.
#' Trim leading and trailing white-space.
#' There are no rows to skip.
#' There is no upper bound on the number of rows to read.
#'
#' @param file A file.
#' @param sheet A sheet.
#' @param range A cell range to read from.
#' @param guess_max Maximum number of rows to use for guessing column types.
#'
#' @return
#'
#' @keywords
#'
#' @import readxl rprojroot
#'
#' @export
#'
#' @examples

import_excel <- function(file,
                         sheet,
                         range,
                         guess_max) {

    root_path <- find_root(criterion = file,
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