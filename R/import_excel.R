#' Read xls and xlsx files.
#'
#' @description
#' Combines \code{readxl::read_excel} and \code{rprojroot::find_root}.
#'
#' Converts upper-case variable names and categorical data to lower-case.
#'
#' @usage
#' import_excel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder Any folder above both 1) the xls / xlsx file and 2) the code file.
#' @param path Relative to \code{folder}, path to the xls / xlsx file.
#' @param sheet Sheet to read.
#' @param range A cell range to read from.
#'
#' @return
#' A base-R data-frame.
#'
#' @seealso
#' \code{\link{import_df}}
#' \code{\link{import_feather}}
#'
#' @importFrom readxl read_excel
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_excel <- function(folder,
                         path,
                         sheet = NULL,
                         range = NULL) {

  stopifnot(is.character(x = folder),
            is.character(x = path),
            (substring(text = path,
                       first = nchar(x = path,
                                     type = "chars",
                                     allowNA = FALSE,
                                     keepNA = TRUE) - 3 + 1,
                       last = nchar(x = path,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == "xls") |
            (substring(text = path,
                       first = nchar(x = path,
                                     type = "chars",
                                     allowNA = FALSE,
                                     keepNA = TRUE) - 4 + 1,
                       last = nchar(x = path,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == "xlsx"))

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
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
                          guess_max = 100000)

  zhaoy::import_df(x = x)

}