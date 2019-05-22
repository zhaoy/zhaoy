#' Frequency of unique values
#'
#' @description
#' Tabulate counts and percents of unique values, including missing-data.
#'
#' @usage
#' s_unique(x)
#'
#' @param x a vector.
#'
#' @return
#' A data-frame.
#'
#' @seealso \code{\link{s_mode} \link{s_s}}
#'
#' @importFrom janitor tabyl
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' zhaoy::s_unique(x = attenu$station)

s_unique <- function(x) {

  dat <- tibble::tibble(value = x,
                        .name_repair = "universal")

  dat <- janitor::tabyl(dat = dat,
                        var1 = value,
                        show_na = TRUE,
                        show_missing_levels = TRUE)

  dat <- janitor::adorn_totals(dat = dat,
                               where = "row",
                               fill = NA,
                               na.rm = FALSE,
                               name = "total")

  dat <- janitor::adorn_pct_formatting(dat = dat,
                                       digits = 1,
                                       rounding = "half to even",
                                       affix_sign = FALSE)

  dat$percent <- as.numeric(x = dat$percent)

  return(value = dat)

}