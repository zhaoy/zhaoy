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
#' @param folder Any folder above both 1) the xls / xlsx file and 2) the R file.
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

  suffix_xls <- "xls"

  suffix_xls_nchar <- nchar(x = suffix_xls,
                            type = "chars",
                            allowNA = FALSE,
                            keepNA = TRUE)

  suffix_xlsx <- "xlsx"

  suffix_xlsx_nchar <- nchar(x = suffix_xlsx,
                             type = "chars",
                             allowNA = FALSE,
                             keepNA = TRUE)

  stopifnot(is.character(x = folder),
            nzchar(x = folder,
                   keepNA = TRUE),
            is.character(x = path),
            ((substring(text = path,
                       first = nchar(x = path,
                                     type = "chars",
                                     allowNA = FALSE,
                                     keepNA = TRUE) - suffix_xls_nchar + 1,
                       last = nchar(x = path,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == suffix_xls) |
            (substring(text = path,
                       first = nchar(x = path,
                                     type = "chars",
                                     allowNA = FALSE,
                                     keepNA = TRUE) - suffix_xlsx_nchar + 1,
                       last = nchar(x = path,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == suffix_xlsx)))

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  import_path <- file.path(root,
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