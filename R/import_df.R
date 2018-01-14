#' Read data-frames.
#'
#' @description
#' Converts column names and categorical data to lower-case.
#'
#' @usage import_df(x)
#'
#' @param range A data-frame.
#'
#' @return A base-R data-frame.
#'
#' @seealso \code{\link{import_excel import_feather}}
#'
#' @importFrom purrr map
#'
#' @export

import_df <- function(x) {

  names(x = x) <- tolower(x = names(x = x))

  x <- purrr::map(.x = x,
                  .f = zhaoy_tolower)

  as.data.frame(x = x,
                row.names = NULL,
                stringsAsFactors = FALSE,
                cut.names = TRUE,
                fix.empty.names = TRUE)

}