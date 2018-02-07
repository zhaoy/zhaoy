#' Read data-frames.
#'
#' @description
#' Convert upper-case variable names and categorical data to lower-case.
#'
#' @usage
#' import_df(x)
#'
#' @param x a data-frame.
#'
#' @return
#' A base-R data-frame.
#'
#' @export
#'
#' @examples
#' import_df(x = iris)

import_df <- function(x) {

  stopifnot((inherits(x = x,
                      what = c("data.frame",
                               "tbl",
                               "tbl_lazy",
                               "tbl_monetdb",
                               "tbl_sql"),
                      which = FALSE) == TRUE) |
            (nrow(x = x) >= 1 &
             ncol(x = x) >= 1))

  if (is.data.frame(x = x) == FALSE) {

    x <- as.data.frame(x = x,
                       row.names = NULL,
                       stringsAsFactors = FALSE,
                       cut.names = TRUE,
                       fix.empty.names = TRUE)

  }

  names(x = x) <- tolower(x = names(x = x))

  x <- lapply(X = x,
              FUN = zhaoy_tolower)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}