#' Character case
#'
#' @description
#' Translate characters from upper to lower case. Leave non-alphabetic characters un-changed.
#'
#' @usage
#' lc_df(x)
#'
#' @param x a data-frame.
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

  names(x = x) <- tolower(x = names(x = x))

  purrr::map_dfc(.x = x,
                 .f = zhaoy_lc_df)

}