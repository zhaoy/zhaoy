#' @import dplyr
#' @importFrom purrr map
#'
#' @export

agg_enc_status_by_p <- function(x) {

  x <- x %>%
  count(mrn,
        appt_status) %>%
  spread(key = appt_status,
         value = n,
         fill = 0,
         drop = TRUE,
         sep = NULL)

  x[, c(names(x = subset(x = x,
                         select = -mrn)))] <- x[, c(names(x = subset(x = x,
                                                                     select = -mrn)))] %>%
  map(.f = as.integer)

  return(value = x)

}