#' @title Export data-frames as Microsoft Excel files
#'
#' @description
#' Export data-frames as Microsoft Excel files.
#'
#' To name sheets, set \code{x} to a named list of data-frames.
#'
#' @usage
#' export_excel(x, dirname, rpath)
#'
#' @param x A data-frame, or named list of data-frames that will be sheets in the Excel file.
#' @param dirname A directory above 1) the Excel file and 2) the R file.
#' @param rpath Relative to \code{dirname}, path to the Excel file.
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

  path <- zhaoy::path(dirname = dirname,
                      rpath = rpath)

  writexl::write_xlsx(x = x,
                      path = path,
                      col_names = TRUE,
                      format_headers = FALSE)

}