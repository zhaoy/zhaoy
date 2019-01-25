#' Character Translation
#'
#' @description
#' Translate characters from upper to lower case.
#'
#' @usage
#' i_df(x)
#'
#' @param x a data-frame.
#'
#' @details
#' Non-alphabetic characters and non-character vectors are left un-changed.
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
#' zhaoy::i_df(x = ToothGrowth)

i_df <- function(x) {

  stopifnot(is.data.frame(x = x) == TRUE)

  names(x = x) <- tolower(x = names(x = x))

  x <- purrr::map(.x = x,
                  .f = zhaoy_lc_df)

  data.frame(x,
             row.names = NULL,
             check.rows = TRUE,
             check.names = TRUE,
             fix.empty.names = TRUE,
             stringsAsFactors = FALSE)

}