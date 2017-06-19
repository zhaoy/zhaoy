#' Read a tsv file into a tibble
#'
#' Read tab separated values, which is among the most common types of flat file data.
#' Execute readr::read_tsv with assumptions about the values of some arguments.
#'
#' Use no characters to quote strings.
#'
#' Use the first row of the input as the column names.
#'
#' Impute all column types from the rows on the input.
#'
#' Use the default locale as the locale.
#'
#' Use no strings for missing values.
#'
#' Treat missing values inside quotes as strings.
#'
#' Use no strings to identify comments.
#'
#' Trim leading and trailing whitespace from each field before parsing it.
#'
#' Skip no lines before reading data.
#'
#' Set the maximum number of records to read, to infinite.
#'
#' Display a progress bar.
#'
#' @param criterion A criterion.
#' @param file Name and extension of a file.
#' @param guess_max Maximum number of records to use for guessing column types.
#'
#' @import readr
#'
#' @export
#'
#' @examples

import_tsv <- function(criterion,
                       file,
                       guess_max) {

    root_path <- find_root(criterion = criterion,
                           path = ".")

    import_path <- file.path(root_path,
                             file,
                             fsep = "/")

    x <- read_tsv(file = import_path,
                  quote = "",
                  col_names = TRUE,
                  col_types = NULL,
                  locale = default_locale(),
                  na = "",
                  quoted_na = FALSE,
                  comment = "",
                  trim_ws = TRUE,
                  skip = 0,
                  n_max = Inf,
                  guess_max = guess_max,
                  progress = TRUE)

    return(value = x)

}