#' @title 2- or 3-way frequency tables of percents
#'
#' @description
#' Cross-tabulate percents of unique values, including missing-data, in 2 or 3 variables.
#'
#' @usage
#' s_cross_p(x, ..., direction)
#'
#' @param x A data-frame.
#' @param ... 2 or 3 variables.
#' @param direction Direction in which to calculate percents: "row", "col", or "all".
#'
#' @return
#' A data-frame.
#'
#' @seealso \code{\link{s_cross_n} \link{s_cross_np}}
#'
#' @importFrom janitor adorn_pct_formatting adorn_percentages adorn_title adorn_totals tabyl
#'
#' @export

s_cross_p <- function(x,
                      ...,
                      direction) {

  dat <- janitor::tabyl(dat = x,
                        show_na = TRUE,
                        show_missing_levels = TRUE,
                        ...)

  dat <- janitor::adorn_totals(dat = dat,
                               where = c("row",
                                         "col"),
                               fill = NA,
                               na.rm = FALSE,
                               name = "total")

  dat <- janitor::adorn_percentages(dat = dat,
                                    denominator = direction,
                                    na.rm = FALSE)

  dat <- janitor::adorn_pct_formatting(dat = dat,
                                       digits = 1,
                                       rounding = "half to even",
                                       affix_sign = FALSE)

  janitor::adorn_title(dat = dat,
                       placement = "combined")

}