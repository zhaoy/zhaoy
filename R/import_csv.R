#' @title Import .csv files
#'
#' @description Import .csv files.
#'
#' @usage import_csv(dirname, rpath, id)
#'
#' @param dirname A directory above both the .csv file and .R file.
#' @param rpath Relative to \code{dirname}, path to the .csv file.
#' @param id Optional: The name of a column in which to store the file path.
#'
#' @returns A data-frame.
#'
#' @importFrom readr read_csv
#'
#' @export

import_csv <- function(dirname,
                       rpath,
                       id = NULL) {
  
  file <- gwep::path(basename = dirname,
                     rpath)
  
  readr::read_csv(file = file,
                  col_names = TRUE,
                  col_types = "c",
                  col_select = NULL,
                  id = id,
                  locale = readr::default_locale(),
                  na = "",
                  quote = "\"",
                  comment = "",
                  trim_ws = TRUE,
                  skip = 0,
                  n_max = Inf,
                  guess_max = 0,
                  name_repair = "minimal",
                  num_threads = readr::readr_threads(),
                  progress = FALSE,
                  show_col_types = FALSE,
                  skip_empty_rows = FALSE,
                  lazy = FALSE)

}