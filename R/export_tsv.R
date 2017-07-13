#' Write a data-frame to a tsv file.
#'
#' @description
#' Executes readr::\code{\link{write_tsv}} with pre-set values in some arguments.
#'
#' @usage
#' \code{export_tsv(file, folder)}
#'
#' @param x A data-frame to write to disk.
#' @param folder Path or connection to folder.
#'
#' @details
#' The readr::\code{\link{write_tsv}} pre-set argument values are
#'
#' \code{na = ""}: Use blanks for missing values.
#'
#' \code{append = FALSE}: Over-write existing file.
#'
#' \code{col_names = TRUE}: Write column names at the top of the file.
#'
#' @return
#' A data-frame.
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

    readr::write_tsv(x = x,
                     path = path,
                     na = "",
                     append = FALSE,
                     col_names = TRUE)

}