#' Write a data frame to a tab-separated-values (tsv) file
#'
#' Never writes row names. Executes readr::write_tsv with assumptions about the values of some arguments.
#'
#' NA is used for missing values.
#'
#' Over-write existing file.
#'
#' Write column names at the top of the file.
#'
#' @param x A data frame to write to disk.
#' @param folder Path or connection to folder.
#'
#' @import readr
#'
#' @export
#'
#' @examples
#' dir <- getwd()
#' export_tsv(x = mtcars, folder = dir)

export_tsv <- function(x,
                       folder) {

    x_name <- substitute(expr = x)

    x_name <- as.character(x = x_name)

    x_name <- paste0(x_name,
                     ".tsv",
                     collapse = NULL)

    path <- file.path(folder,
                      x_name,
                      fsep = "/")

    write_tsv(x = x,
              path = path,
              na = "",
              append = FALSE,
              col_names = TRUE)

}