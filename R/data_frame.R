data_frame <- function(x) {
  data.frame(x,
             row.names = NULL,
             check.rows = TRUE,
             check.names = TRUE,
             fix.empty.names = TRUE,
             stringsAsFactors = FALSE)
}