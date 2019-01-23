#' Summary Statistics
#'
#' @description
#' Tabulate summary statistics.
#'
#' @usage
#' s_s(x)
#'
#' @param x an R object.
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
#' @importFrom dplyr combine n_distinct select
#' @importFrom purrr map_chr map_dbl map_int modify_at
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' s_s(x = attenu$mag)
#' s_s(x = attenu)

s_s <- function(x) {

  options(tibble.print_max = Inf,
          tibble.width = Inf)

  stopifnot(inherits(x = x,
                     what = dplyr::combine("character",
                                           "integer",
                                           "logical",
                                           "numeric",
                                           "factor",
                                           "Date",
                                           "difftime",
                                           "POSIXct",
                                           "POSIXlt",
                                           "data.frame"),
                     which = FALSE))

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

  min <- purrr::map_chr(.x = x,
                        .f = zhaoy_s_s,
                        s = "min")

  max <- purrr::map_chr(.x = x,
                        .f = zhaoy_s_s,
                        s = "max")

  median <- purrr::map_dbl(.x = x,
                           .f = zhaoy_s_s,
                           s = "median")

  mode <- purrr::map_chr(.x = x,
                         .f = zhaoy_s_mode)

  mean <- purrr::map_dbl(.x = x,
                         .f = zhaoy_s_s,
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
                          .at = dplyr::combine("pct_miss",
                                               "pct_unique"),
                          .f = round,
                          digits = 1)

  s_s$min[is.na(x = s_s$min) == FALSE] <- prettyNum(x = s_s$min[is.na(x = s_s$min) == FALSE],
                                                    drop0trailing = TRUE)

  s_s$max[is.na(x = s_s$max) == FALSE] <- prettyNum(x = s_s$max[is.na(x = s_s$max) == FALSE],
                                                    drop0trailing = TRUE)

  s_s$mode[is.na(x = s_s$mode) == FALSE] <- prettyNum(x = s_s$mode[is.na(x = s_s$mode) == FALSE],
                                                      drop0trailing = TRUE)

  if (nrow(x = s_s) == 1) {

    dplyr::select(.data = s_s,
                  -var)

  } else if (nrow(x = s_s) > 1) {

    s_s

  }

}