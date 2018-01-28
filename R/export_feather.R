#' Write data-frames to Feather files.
#'
#' @description
#' Combines \code{feather::write_feather} and \code{rprojroot::find_root}.
#'
#' @usage
#' export_feather(x, folder, path)
#'
#' @param x A data-frame to write to disk.
#' @param folder Any folder above both 1) the path to write to and 2) the code file.
#' @param path Relative to \code{folder}, path to write to.
#'
#' @return
#' A data-frame.
#'
#' @importFrom feather write_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

export_feather <- function(x,
                           folder,
                           path) {

  stopifnot(is.data.frame(x = x))

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  export_path <- file.path(root_path,
                           path,
                           fsep = "/")

  # Because zhaoy::import_feather generates base-R data-frames,
  # it is tolerable for zhaoy::export_feather to potentially generate tibbles.

  feather::write_feather(x,
                         path = export_path)

}