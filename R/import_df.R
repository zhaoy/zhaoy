#' Read R-package data-frames.
#'
#' @description
#' Converts upper-case column names and categorical data to lower-case.
#'
#' @usage
#' import_df(x)
#'
#' @param x A R-package data-frame.
#'
#' @return
#' A base-R data-frame.
#'
#' @seealso
#' \code{\link{import_excel}}
#' \code{\link{import_feather}}
#'
#' @export
#'
#' @examples
#' import_df(x = iris)

import_df <- function(x) {

  stopifnot(is.data.frame(x = x))

  names(x = x) <- tolower(x = names(x = x))

  x <- lapply(X = x,
              FUN = zhaoy_tolower)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}