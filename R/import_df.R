#' Convert data-frame characters to lower-case.
#'
#' @description
#' Convert characters from upper to lower case, in variable names and in data points.
#'
#' @usage
#' import_df(x)
#'
#' @param x a data-frame.
#'
#' @return
#' A data-frame.
#'
#' @importFrom purrr map
#'
#' @export
#'
#' @examples
#' ToothGrowth$supp <- as.character(x = ToothGrowth$supp)
#' import_df(x = ToothGrowth)

import_df <- function(x) {

  stopifnot(inherits(x = x,
                     what = c("data.frame",
                              "tbl_lazy",
                              "tbl_monetdb",
                              "tbl_sql"),
                     which = FALSE) == TRUE,
            nrow(x = x) >= 1,
            ncol(x = x) >= 1)

  if (is.data.frame(x = x) == FALSE) {

    x <- as.data.frame(x = x,
                       row.names = NULL,
                       stringsAsFactors = FALSE,
                       cut.names = TRUE,
                       fix.empty.names = TRUE)

  }

  names(x = x) <- tolower(x = names(x = x))

  x <- purrr::map(.x = x,
                  .f = zhaoy_lc_df)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}