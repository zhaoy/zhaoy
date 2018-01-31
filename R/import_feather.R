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
#' @param folder Any folder above both 1) the Feather file and 2) the R file.
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

  suffix <- "feather"

  suffix_nchar <- nchar(x = suffix,
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
                                     keepNA = TRUE) - suffix_nchar + 1,
                       last = nchar(x = path,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == suffix)))

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  import_path <- file.path(root,
                           path,
                           fsep = "/")

  x <- feather::read_feather(path = import_path)

  zhaoy::import_df(x = x)

}