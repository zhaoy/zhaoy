#' Summarize values of vectors
#'
#' @description
#' Tabulate 1) values of vectors in ascending order, beginning with any NAs,
#' 2) counts, and 3) percentages.
#'
#' @usage
#' \code{s_value(x)}
#'
#' @param x a numeric, complex, character, or logical vector.
#'
#' @return A data-frame.
#'
#' @seealso \code{\link{s_col}}
#'
#' @export
#'
#' @examples
#' s_value(x = warpbreaks$breaks)

s_value <- function(x) {

  df <- data.frame(value = x,
                   row.names = NULL,
                   check.rows = TRUE,
                   check.names = TRUE,
                   fix.empty.names = TRUE,
                   stringsAsFactors = FALSE)

  count <- table(df$value,
                 useNA = "ifany")

  percent <- prop.table(x = count,
                        margin = NULL) * 100

  df <- data.frame(count,
                   percent,
                   row.names = NULL,
                   check.rows = TRUE,
                   check.names = TRUE,
                   fix.empty.names = TRUE,
                   stringsAsFactors = FALSE)

  df <- df[, c("Var1",
               "Freq",
               "Freq.1")]

  names(x = df)[names(x = df) == "Var1"] <- "value"

  names(x = df)[names(x = df) == "Freq"] <- "count"

  names(x = df)[names(x = df) == "Freq.1"] <- "percent"

  df[order(df$value,
           decreasing = FALSE,
           na.last = FALSE), ]

}