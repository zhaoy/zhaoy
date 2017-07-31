#' Summarize values of vectors
#'
#' @description
#' Tabulate 1) values of vectors in ascending order, beginning with any NAs,
#' 2) counts, and 3) percentages.
#'
#' @usage
#' \code{s_col(x)}
#'
#' @param x a numeric, complex, character, or logical vector.
#'
#' @return A data-frame.
#'
#' @seealso \code{\link{s_tbl}}
#'
#' @export
#'
#' @examples
#' s_col(x = warpbreaks$breaks)

s_col <- function(x) {

  x <- data.frame(value = x,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  count <- table(x$value,
                 useNA = "ifany")

  percent <- prop.table(x = count,
                        margin = NULL) * 100

  x <- data.frame(count,
                  percent,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  x <- x[, c("Var1",
             "Freq",
             "Freq.1")]

  names(x = x)[names(x = x) == "Var1"] <- "value"

  names(x = x)[names(x = x) == "Freq"] <- "count"

  names(x = x)[names(x = x) == "Freq.1"] <- "percent"

  x[order(x$value,
          decreasing = FALSE,
          na.last = FALSE), ]

}