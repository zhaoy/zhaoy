#' Read Feather files.
#'
#' @description
#' Executes feather::\code{\link{read_feather}}.
#' Converts column names and categorical data to lower-case.
#'
#' @usage
#' import_feather(folder, path)
#'
#' @param folder Any folder above both 1) the Feather file and 2) the file that contains the code.
#' @param path Path to the Feather file, excluding \code{folder}.
#'
#' @return A base-R data-frame.
#'
#' @seealso \code{\link{import_excel}}
#'
#' @importFrom feather read_feather
#' @importFrom purrr map
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

import_feather <- function(folder,
                           path) {

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  full_path <- file.path(root_path,
                         path,
                         fsep = "/")

  x <- feather::read_feather(path = full_path)

  names(x = x) <- tolower(x = names(x = x))

  x <- purrr::map(.x = x,
                  .f = zhaoy_tolower)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}