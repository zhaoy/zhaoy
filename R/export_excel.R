#' Export to xlsx
#'
#' @description
#' Write a data-frame to an xlsx file. Support strings, numbers, booleans, and dates.
#'
#' @usage
#' export_excel(x, dirname, rpath)
#'
#' @param x data-frame, or named list of data-frames that are sheets in the future xlsx file.
#' @param dirname a directory above both 1) the future xlsx file and 2) the R file.
#' @param rpath relative to \code{dirname}, the path to the future xlsx file.
#'
#' @seealso
#' \code{\link{import_excel}}
#'
#' @importFrom writexl write_xlsx
#'
#' @export

export_excel <- function(x,
                         dirname,
                         rpath) {
  
  path <- zhaoy::file_path(dirname = dirname,
                           rpath = rpath)
  
  writexl::write_xlsx(x = x,
                      path = path,
                      col_names = TRUE,
                      format_headers = FALSE)
  
}