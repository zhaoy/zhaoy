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

  x <- as.data.frame(x = x,
                     row.names = NULL,
                     stringsAsFactors = FALSE,
                     cut.names = TRUE,
                     fix.empty.names = TRUE)

  n <- table(x[, 1],
             useNA = "ifany")

  pct <- prop.table(x = n) * 100

  pct <- round(x = pct,
               digits = 1)

  x <- data.frame(n,
                  pct,
                  row.names = NULL,
                  check.rows = TRUE,
                  check.names = TRUE,
                  fix.empty.names = TRUE,
                  stringsAsFactors = FALSE)

  x <- x[, c("Var1",
             "Freq",
             "Freq.1")]

  names(x = x)[names(x = x) == "Var1"] <- "value"

  names(x = x)[names(x = x) == "Freq"] <- "n"

  names(x = x)[names(x = x) == "Freq.1"] <- "pct"

  x[order(x$value,
          decreasing = FALSE,
          na.last = FALSE), ]

}