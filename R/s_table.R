s_table <- function(x,
                    var1,
                    var2 = NULL,
                    var3 = NULL) {

  x <- janitor::tabyl(dat = x,
                      var1,
                      var2 = var2,
                      var3 = var3,
                      show_na = TRUE,
                      show_missing_levels = TRUE)

  x <- janitor::adorn_totals(dat = x,
                             where = c("row",
                                       "col"),
                             fill = NA,
                             na.rm = FALSE,
                             name = "total")

  denominator <- 1

  x <- janitor::adorn_percentages(dat = x,
                                  denominator = denominator,
                                  na.rm = FALSE)

  x <- janitor::adorn_pct_formatting(dat = x,
                                     digits = 1,
                                     rounding = "half to even",
                                     affix_sign = TRUE)

  position <- 1

  x <- janitor::adorn_ns(dat = x,
                         position = position,
                         ns = attr(x = x,
                                   which = "core"))

  x <- janitor::adorn_title(dat = x,
                            placement = "combined")

}