#' Write data-frames to Feather files.
#'
#' @description
#' Combines \code{feather::write_feather} and \code{rprojroot::find_root}.
#'
#' @usage
#' export_feather(x, folder, path)
#'
#' @param x A data-frame to write to disk.
#' @param folder Any folder above both 1) the path to write to and 2) the R file.
#' @param path Relative to \code{folder}, path to write to.
#'
#' @importFrom feather write_feather
#' @importFrom rprojroot find_root has_dirname
#'
#' @export

export_feather <- function(x,
                           folder,
                           path) {

  suffix <- "feather"

  suffix_nchar <- nchar(x = suffix,
                        type = "chars",
                        allowNA = FALSE,
                        keepNA = TRUE)

  stopifnot(is.data.frame(x = x),
            nrow(x = x) >= 1,
            ncol(x = x) >= 1,
            is.character(x = folder),
            nzchar(x = folder,
                   keepNA = TRUE),
            is.character(x = path),
            (substring(text = path,
                       first = nchar(x = path,
                                     type = "chars",
                                     allowNA = FALSE,
                                     keepNA = TRUE) - suffix_nchar + 1,
                       last = nchar(x = path,
                                    type = "chars",
                                    allowNA = FALSE,
                                    keepNA = TRUE)) == suffix))

  root <- rprojroot::find_root(criterion = rprojroot::has_dirname(dirname = folder),
                               path = ".")

  export_path <- file.path(root,
                           path,
                           fsep = "/")

  # Because zhaoy::import_feather generates base-R data-frames,
  # it is tolerable for zhaoy::export_feather to potentially generate tibbles.

  feather::write_feather(x,
                         path = export_path)

}