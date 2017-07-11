#' Read a tsv into a tibble.
#'
#' @description
#' Read tab-separated values.
#' Execute readr::\code{\link{read_tsv}} with with pre-set values in some arguments.
#' Convert column names to lower-case.
#'
#' @usage
#' \code{read_tsv(criterion, file, guess_max = 10)}
#'
#' @param criterion \code{criterion} in rprojroot::\code{\link{find_root}}.
#' @param file Path to a file, excluding the root directory.
#' @param guess_max Maximum number of records to use for guessing column types.
#'
#' @details
#' The readr::\code{\link{read_tsv}} pre-set argument values are
#'
#' \code{quote = ""}: Use blanks to quote strings.
#'
#' \code{colnames = TRUE}: Use the first row of the input as the column names.
#'
#' \code{col_types = NULL}: Impute all column types from the first 1000 rows on the input.
#'
#' \code{locale = default_locale()}: Use readr::\code{\link{default_locale}} as the locale.
#'
#' \code{na = ""}: Use blanks for missing values.
#'
#' \code{quoted_na = FALSE}: Treat missing values inside quotes as strings.
#'
#' \code{comment = ""}: Use blanks to identify comments.
#'
#' \code{trim_ws = TRUE}: Trim leading and trailing whitespace from each field before parsing it.
#'
#' \code{skip = 0}: Skip no lines before reading data.
#'
#' \code{n_max = Inf}: Set the maximum number of records to read, to \code{\link{Inf}}.
#'
#' \code{progress = TRUE}: Display a progress bar.
#'
#' @return
#' A data frame.
#'
#' @seealso \code{\link{import_excel}}
#'
#' @import readr rprojroot
#'
#' @export
#'
#' @examples

import_tsv <- function(criterion,
                       file,
                       guess_max) {

    root_path <- rprojroot::find_root(criterion = criterion,
                                      path = ".")

    import_path <- file.path(root_path,
                             file,
                             fsep = "/")

    x <- readr::read_tsv(file = import_path,
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

    names(x = x) <- tolower(x = names(x = x))

    return(value = x)

}