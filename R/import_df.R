#' Read R data-frames.
#'
#' @description
#' Converts upper-case variable names and categorical data to lower-case.
#'
#' @usage
#' import_df(x)
#'
#' @param x A data-frame.
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

  stopifnot(is.data.frame(x = x),
            nrow(x = x) >= 1,
            ncol(x = x) >= 1)

  names(x = x) <- tolower(x = names(x = x))

  x <- lapply(X = x,
              FUN = zhaoy_tolower)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}