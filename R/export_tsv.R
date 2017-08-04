#' Write a data-frame to a tsv file.
#'
#' @description
#' Execute readr::\code{\link{write_tsv}} with pre-set values in some arguments.
#'
#' @usage
#' \code{export_tsv(x, folder)}
#'
#' @param x A data-frame to write to disk.
#' @param folder Path or connection to the folder that will store the data-frame.
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
#' @return \code{export_*()} returns the input x invisibly.
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

  path <- file.path(x_name,
                    ".tsv",
                    fsep = "")

  path <- file.path(folder,
                    path,
                    fsep = "/")

  readr::write_tsv(x = x,
                   path = path,
                   na = "",
                   append = FALSE,
                   col_names = TRUE)

}