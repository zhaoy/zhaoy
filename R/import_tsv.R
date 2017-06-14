#' Import tab-separated-values (tsv) files
#'
#' Execute readr::read_tsv with assumptions about values of some arguments.
#'
#' First rows are column names.
#' Guess column types.
#' Only blank cells represent missing data.
#' Missing data inside quotes constitute missing data.
#' There are no characters for quoting strings.
#' There are no strings for identifying comments.
#' Trim white-space.
#' There are no rows to skip.
#' There is no upper bound on the number of rows to read.
#' Show progress bar.
#' locale = default_locale()
#'
#' @param path A path.
#' @param guess_max Maximum number of rows to use for guessing column types.
#'
#' @return
#'
#' @keywords
#'
#' @import readr
#'
#' @export
#'
#' @examples

import_tsv <- function(path,
                       guess_max) {

    x <- read_tsv(file = path,
                  col_names = TRUE,
                  col_types = NULL,
                  locale = default_locale(),
                  na = "",
                  quoted_na = TRUE,
                  quote = "",
                  comment = "",
                  trim_ws = TRUE,
                  skip = 0,
                  n_max = Inf,
                  guess_max = guess_max,
                  progress = TRUE)

    return(value = x)

}