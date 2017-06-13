#' Export tab-separated-values (tsv) files
#'
#' Execute readr::write_tsv with assumptions about values of some arguments.
#'
#' Only blank cells represent missing data.
#' Over-write existing file if one exists.
#' Write column names at top of file.
#'
#' @param tbl A table.
#' @param path A path.
#'
#' @return
#'
#' @keywords
#'
#' @import readr
#'
#' @export
#'
#' @examples

export_tsv <- function(tbl,
                       path) {

    write_tsv(x = tbl,
              path = path,
              na = "",
              append = FALSE,
              col_names = TRUE)

}