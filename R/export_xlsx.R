#' @title Export data-frames as .xlsx files
#'
#' @description
#' Export data-frames as .xlsx files.
#'
#' To export data-frames as named sheets, set \code{x} to a named list of data-frames.
#'
#' @usage
#' export_xlsx(x, dirname, rpath)
#'
#' @param x A data-frame, or named list of data-frames that will be sheets in the .xlsx file.
#' @param dirname A directory above both the .xlsx file and .R file.
#' @param rpath Relative to \code{dirname}, path to the .xlsx file.
#'
#' @returns \code{export_xlsx()} returns \code{x} invisibly.
#'
#' @importFrom writexl write_xlsx
#'
#' @export

export_xlsx <- function(x,
                        dirname,
                        rpath) {

  path <- gwep::path(basename = dirname,
                     rpath)

  writexl::write_xlsx(x = x,
                      path = path,
                      col_names = TRUE,
                      format_headers = FALSE,
                      use_zip64 = FALSE)

}