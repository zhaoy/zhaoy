#' Write data-frames to Feather files.
#'
#' @description
#' Write data-frames to Feather files.
#'
#' @usage
#' export_feather(x, folder, path)
#'
#' @param x a data-frame to write to \code{path}.
#' @param folder any folder above both 1) the path to write to and 2) the R file.
#' @param path relative to \code{folder}, path to write to.
#'
#' @seealso
#' \code{\link{import_feather}}
#'
#' @importFrom feather write_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

export_feather <- function(x,
                           folder,
                           path) {

  # is.character() is necessary because
  # nzchar() and grepl() can return TRUE / FALSE
  # for non-character inputs.

  stopifnot(is.character(x = folder),
            nzchar(x = folder,
                   keepNA = TRUE),
            is.character(x = path),
            grepl(pattern = "[.feather]",
                  x = path,
                  ignore.case = TRUE))

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  export_path <- file.path(root,
                           path,
                           fsep = "/")

  # Because zhaoy::import_feather generates base-R data-frames,
  # it is tolerable for zhaoy::export_feather to generate tibbles.

  feather::write_feather(x,
                         path = export_path)

}