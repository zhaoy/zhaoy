#' Read xls and xlsx files.
#'
#' @description
#' Executes readxl::\code{\link{read_excel}} with pre-set values in some arguments.
#' Uses relative file paths.
#' Converts column names and categorical data to lower-case.
#'
#' @usage
#' import_excel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder Any folder above both 1) the xls / xlsx file and 2) the file that contains the code.
#' @param path Path to the xls / xlsx file, excluding \code{folder}.
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#'
#' @details
#' \code{import_excel} excutes readxl::\code{\link{read_excel}} with the following pre-set argument values:
#'
#' \code{col_names = TRUE}: Use the first row as column names.
#'
#' \code{col_types = NULL}: Guess all from the spread-sheet.
#'
#' \code{na = ""}: Treat blank cells as missing data.
#'
#' \code{trim_ws = TRUE}: Trim leading and trailing white-space.
#'
#' \code{skip = 0}: Skip a minimum of 0 rows before reading anything.
#'
#' \code{n_max = Inf}: Read a maximum of data rows.
#'
#' \code{guess_max = 100000}: Use a maximum of 100000 data rows to guess column types.
#'
#' @return A base-R data-frame.
#'
#' @seealso \code{\link{import_feather}}
#'
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_excel <- function(folder,
                         path,
                         sheet = NULL,
                         range = NULL) {

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  full_path <- file.path(root_path,
                         path,
                         fsep = "/")

  x <- readxl::read_excel(path = full_path,
                          sheet = sheet,
                          range = range,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          trim_ws = TRUE,
                          skip = 0,
                          n_max = Inf,
                          guess_max = 100000)

  names(x = x) <- tolower(x = names(x = x))

  x <- purrr::map(.x = x,
                  .f = zhaoy_tolower)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}