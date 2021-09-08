#' @title Trim excess space from strings
#'
#' @description
#' Trim excess space from strings.
#'
#' @usage
#' squish_df(x)
#'
#' @param x A data-frame.
#'
#' @return
#' A tibble.
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom stringr str_squish
#'
#' @export
#'
#' @examples
#' ToothGrowth$supp <- as.character(x = ToothGrowth$supp)
#' zhaoy::squish_df(x = ToothGrowth)

squish_df <- function(x) {

  options(tibble.print_max = Inf,
          tibble.width = Inf)

  dplyr::mutate(.data = x,
                dplyr::across(.cols = where(fn = is.character),
                              .fns = stringr::str_squish,
                              .names = "{.col}"))

}