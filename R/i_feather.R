#' Read Feather files.
#'
#' @description
#' Read Feather files.
#'
#' @usage
#' i_feather(folder, path)
#'
#' @param folder any folder above both 1) the Feather file and 2) the R file.
#' @param path relative to \code{folder}, path to the Feather file.
#'
#' @return
#' A base-R data-frame.
#'
#' @seealso
#' \code{\link{export_feather}}
#'
#' @importFrom feather read_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

i_feather <- function(folder,
                           path) {

  # is.character() is necessary because
  # nzchar() can return TRUE or FALSE
  # for non-character inputs.

  stopifnot(is.character(x = folder),
            nzchar(x = folder,
                   keepNA = TRUE),
            is.character(x = path))

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  import_path <- file.path(root,
                           path,
                           fsep = "/")

  x <- feather::read_feather(path = import_path)

  zhaoy::import_df(x = x)

}