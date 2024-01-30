#' @title Convert upper-case English characters to lower-case
#'
#' @description
#' Convert upper-case English characters to lower-case.
#'
#' @usage
#' i_df(x)
#'
#' @param x A data-frame.
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

  names(x = x) <- tolower(x = names(x = x))

  x <- purrr::map(.x = x,
                  .f = internal_lc_df)

  data.frame(x,
             row.names = NULL,
             check.rows = TRUE,
             check.names = TRUE,
             fix.empty.names = TRUE,
             stringsAsFactors = FALSE)

}