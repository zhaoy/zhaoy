#' Tidy Provide Enterprise ADAP data.
#'
#' @description
#' Tidy the Provide Enterprise "Clients - View ADAP Clients by Recertification Date" report.
#'
#' @usage
#' tidy_adap(x)
#'
#' @param x A data-frame.
#'
#' @return
#' A tibble.
#'
#' @importFrom dplyr filter mutate select
#' @importFrom lubridate mdy
#' @importFrom tidyr fill
#'
#' @export

tidy_adap <- function(x) {
  
  # Rename variables.
  
  names(x = x) <- c("v_1",
                    "v_2")
  
  # Create the "recert_date" variable.
  
  x <- dplyr::mutate(.data = x,
                     recert_date = NA)
  
  x$recert_date[is.na(x = x$v_1) == FALSE &
                grepl(pattern = "((\\d){2})/((\\d){2})/((\\d){4})",
                      x = x$v_1,
                      fixed = FALSE) == TRUE] <- x$v_1[is.na(x = x$v_1) == FALSE &
                                                       grepl(pattern = "((\\d){2})/((\\d){2})/((\\d){4})",
                                                             x = x$v_1,
                                                             fixed = FALSE) == TRUE]
  
  # In the "recert_date" variable, convert data to dates.
  
  x$recert_date <- lubridate::mdy(x = x$recert_date)
  
  # Create the "adap_type" variable.
  
  x <- dplyr::mutate(.data = x,
                     adap_type = NA)
  
  x$adap_type[is.na(x = x$v_1) == FALSE &
              grepl(pattern = "^((direct dispense)|insurance|map)$",
                    x = x$v_1,
                    ignore.case = TRUE,
                    fixed = FALSE) == TRUE] <- x$v_1[is.na(x = x$v_1) == FALSE &
                                                     grepl(pattern = "^((direct dispense)|insurance|map)$",
                                                           x = x$v_1,
                                                           ignore.case = TRUE,
                                                           fixed = FALSE) == TRUE]
  
  # Create the "adap_id" variable.
  
  x <- dplyr::mutate(.data = x,
                     adap_id = NA)
  
  x$adap_id[is.na(x = x$v_2) == FALSE &
            grepl(pattern = "^((\\d)+)$",
                  x = x$v_2,
                  fixed = FALSE) == TRUE] <- x$v_2[is.na(x = x$v_2) == FALSE &
                                                   grepl(pattern = "^((\\d)+)$",
                                                         x = x$v_2,
                                                         fixed = FALSE) == TRUE]
  
  # In certain variables, fill in missing-data.
  
  x <- tidyr::fill(data = x,
                   recert_date,
                   adap_type,
                   .direction = "down")
  
  # In the "adap_id" variable, include only ADAP-ID data.
  
  x <- dplyr::filter(.data = x,
                     is.na(x = adap_id) == FALSE)
  
  # Select variables.
  
  dplyr::select(.data = x,
                adap_id,
                recert_date,
                adap_type)

}