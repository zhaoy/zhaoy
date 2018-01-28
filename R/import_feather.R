#' Read Feather files.
#'
#' @description
#' Combines \code{feather::read_feather} and \code{rprojroot::find_root}.
#'
#' Converts upper-case variable names and categorical data to lower-case.
#'
#' @usage
#' import_feather(folder, path)
#'
#' @param folder Any folder above both 1) the Feather file and 2) the code file.
#' @param path Relative to \code{folder}, path to the Feather file.
#'
#' @return
#' A base-R data-frame.
#'
#' @seealso
#' \code{\link{import_df}}
#' \code{\link{import_excel}}
#'
#' @importFrom feather read_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_feather <- function(folder,
                           path) {

  stopifnot(is.character(x = folder),
            is.character(x = path),
            (substring(text = x,
                       first = nchar(x = x,
                                     type = "chars",
                                     allowNA = FALSE,
                                     keepNA = TRUE) - 7 + 1,
                       last = nchar(x = x,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == "feather"))

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  import_path <- file.path(root_path,
                           path,
                           fsep = "/")

  x <- feather::read_feather(path = import_path)

  zhaoy::import_df(x = x)

}