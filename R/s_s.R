#' @title Summary Statistics
#'
#' @description
#' Tabulate summary statistics.
#'
#' @usage
#' s_s(x)
#'
#' @param x An R object.
#'
#' @return
#' A tibble.
#'
#' If \code{x}, or columns in \code{x}, are missing-data, the min / max / median / mode / mean are \code{NA}s.
#'
#' min and max: If \code{x}, or columns in \code{x}, are not numeric or date / date-time objects, the results are \code{NA}s.
#'
#' median and mean: If \code{x}, or columns in \code{x}, are not numeric, the results are \code{NA}s.
#'
#' mode:
#'
#' If the mode is missing-data, the result is \code{NA}.
#'
#' If multiple modes exist, the result is "s_mode()".
#'
#' @seealso \code{\link{s_mode} \link{s_unique}}
#'
#' @importFrom dplyr n_distinct select vars
#' @importFrom purrr map_chr map_dbl map_int modify_at
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' zhaoy::s_s(x = attenu$mag)
#' zhaoy::s_s(x = attenu)

s_s <- function(x) {

  options(tibble.print_max = Inf,
          tibble.width = Inf)

  stopifnot(inherits(x = x,
                     what = c("character",
                              "integer",
                              "logical",
                              "numeric",
                              "factor",
                              "Date",
                              "difftime",
                              "POSIXct",
                              "POSIXlt",
                              "data.frame"),
                     which = FALSE) == TRUE)

  if (is.data.frame(x = x) == FALSE) {

    x <- tibble::tibble(x,
                        .rows = NULL,
                        .name_repair = "universal")

  }

  var <- names(x = x)

  n_miss <- purrr::map_int(.x = x,
                           .f = ~ sum(is.na(x = .),
                                      na.rm = FALSE))

  pct_miss <- n_miss / nrow(x = x) * 100

  n_unique <- purrr::map_int(.x = x,
                             .f = dplyr::n_distinct,
                             na.rm = FALSE)

  pct_unique <- n_unique / nrow(x = x) * 100

  min <- purrr::map(.x = x,
                    .f = internal_s_s,
                    s = "min")

  min <- purrr::map_chr(.x = min,
                        .f = as.character)

  max <- purrr::map(.x = x,
                    .f = internal_s_s,
                    s = "max")

  max <- purrr::map_chr(.x = max,
                        .f = as.character)

  median <- purrr::map_dbl(.x = x,
                           .f = internal_s_s,
                           s = "median")

  mode <- purrr::map(.x = x,
                     .f = internal_s_mode)

  mode <- purrr::map_chr(.x = mode,
                         .f = as.character)

  mean <- purrr::map_dbl(.x = x,
                         .f = internal_s_s,
                         s = "mean")

  s_s <- tibble::tibble(var,
                        n_miss,
                        pct_miss,
                        n_unique,
                        pct_unique,
                        min,
                        max,
                        median,
                        mode,
                        mean,
                        .rows = NULL,
                        .name_repair = "universal")

  s_s <- purrr::modify_at(.x = s_s,
                          .at = dplyr::vars(pct_miss,
                                            pct_unique,
                                            median,
                                            mean),
                          .f = round,
                          digits = 1)

  if (nrow(x = s_s) == 1) {

    dplyr::select(.data = s_s,
                  -var)

  } else if (nrow(x = s_s) > 1) {

    s_s

  }

}