#' @title 2- or 3-way frequency tables of counts
#'
#' @description
#' Cross-tabulate counts of unique values, including missing-data, in 2 or 3 variables.
#'
#' @usage
#' s_cross_n(x, ...)
#'
#' @param x A data-frame.
#' @param ... 2 or 3 variables.
#'
#' @return
#' A data-frame.
#'
#' @seealso \code{\link{s_cross_p} \link{s_cross_np}}
#'
#' @importFrom janitor adorn_title adorn_totals tabyl
#'
#' @export
#'
#' @examples

s_cross_n <- function(x,
                      ...) {

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

  janitor::adorn_title(dat = dat,
                       placement = "combined")

}