#' @export

data_table <- function(x,
                       order_by = NULL) {

  datatable(data = x,
            class = "cell-border hover stripe",
            filter = list(clear = TRUE,
                          position = "top"),
            options = list(autoWidth = TRUE,
                           order = list(order_by,
                                        "desc"),
                           pageLength = 5,
                           regex = FALSE,
                           caseInsensitive = TRUE,
                           searchHighlight = TRUE),
            rownames = FALSE)

}