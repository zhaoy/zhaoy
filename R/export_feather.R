#' Write Feather files.
#'
#' @description
#' Combines \code{feather::write_feather} and \code{rprojroot::find_root}.
#'
#' @usage
#' export_feather(x, folder, path)
#'
#' @param x A data-frame to write to disk.
#' @param folder Any folder above both 1) the Feather file and 2) the code file.
#' @param path Relative to \code{folder}, path to the Feather file.
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

  full_path <- file.path(root_path,
                         path,
                         fsep = "/")

  # Because imports result in base-R data-frames,
  # it is acceptable for exports to result in tibbles or other data-frames.

  feather::write_feather(x,
                         path = full_path)

}