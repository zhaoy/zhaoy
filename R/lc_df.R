#' Character Translation
#'
#' @description
#' Translate characters from upper to lower case.
#'
#' @usage
#' lc_df(x)
#'
#' @param x a data-frame.
#'
#' @details
#' Non-alphabetic characters and non-character vectors are left un-changed.
#'
#' @return
#' A tibble.
#'
#' @importFrom purrr map_dfc
#'
#' @export
#'
#' @examples
#' ToothGrowth$supp <- as.character(x = ToothGrowth$supp)
#' zhaoy::lc_df(x = ToothGrowth)

lc_df <- function(x) {

  options(tibble.print_max = Inf,
          tibble.width = Inf)

  stopifnot(inherits(x = x,
                     what = "data.frame",
                     which = FALSE) == TRUE)

  names(x = x) <- tolower(x = names(x = x))

  purrr::map_dfc(.x = x,
                 .f = zhaoy_lc_df)

}