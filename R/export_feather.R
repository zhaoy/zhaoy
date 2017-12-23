#' Write Feather files.
#'
#' @description
#' Executes feather::\code{\link{write_feather}}.
#' Uses relative file-paths.
#'
#' @usage
#' export_feather(x, folder, path)
#'
#' @param x A data-frame to write to disk.
#' @param folder Any folder above both 1) the Feather file and 2) the file that contains the code.
#' @param path Path to the Feather file, excluding \code{folder}.
#'
#' @return A base-R data-frame.
#'
#' @importFrom feather write_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

export_feather <- function(x,
                           folder,
                           path) {

  root_path <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                                    path = ".")

  full_path <- file.path(root_path,
                         path,
                         fsep = "/")

  feather::write_feather(x,
                         path = full_path)

}