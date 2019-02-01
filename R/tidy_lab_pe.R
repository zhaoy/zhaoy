#' Tidy Provide Enterprise laboratory data.
#'
#' @description
#' Tidy the Provide Enterprise "Test Results by Client with ID" report.
#'
#' @usage
#' tidy_lab_pe(x)
#'
#' @param x a data-frame.
#'
#' @return
#' A tibble.
#'
#' @importFrom dplyr case_when filter select
#' @importFrom purrr map
#' @importFrom tidyr fill
#'
#' @export

tidy_lab_pe <- function(x) {

  lab_pe <- x

  names(x = lab_pe) <- c("x_1",
                         "x_2",
                         "x_3",
                         "x_4")

  lab_pe <- dplyr::filter(.data = lab_pe,
                          x_1 %in% c("test name",
                                     "1") == FALSE &
                          x_2 != "client id:" &
                          grepl(pattern = "total tests for this client:",
                                x = lab_pe$x_1,
                                fixed = TRUE) == FALSE)

# mrn

  lab_pe$mrn <- dplyr::case_when(grepl(pattern = "client id:",
                                       x = lab_pe$x_2,
                                       fixed = TRUE) == TRUE ~
                                 lab_pe$x_2,
                                 grepl(pattern = "client id:",
                                       x = lab_pe$x_2,
                                       fixed = TRUE) == FALSE ~
                                 NA_character_)

  lab_pe$mrn[is.na(x = lab_pe$mrn) == FALSE] <- strsplit(x = lab_pe$mrn[is.na(x = lab_pe$mrn) == FALSE],
                                                         split = "\\s{2}",
                                                         fixed = FALSE)

  lab_pe$mrn[is.na(x = lab_pe$mrn) == FALSE] <- purrr::map(.x = lab_pe$mrn[is.na(x = lab_pe$mrn) == FALSE],
                                                           .f = 2)

  lab_pe$mrn <- c(lab_pe$mrn,
                  recursive = TRUE)

  lab_pe <- tidyr::fill(data = lab_pe,
                        mrn,
                        .direction = "down")

  lab_pe$mrn <- zhaoy::lz_id(x = lab_pe$mrn,
                             lz = TRUE)

  lab_pe <- dplyr::filter(.data = lab_pe,
                          grepl(pattern = "client id:",
                                x = lab_pe$x_2,
                                fixed = TRUE) == FALSE)

# lab_name

  names(x = lab_pe)[names(x = lab_pe) == "x_1"] <- "lab_name"

# result_date

  lab_pe$x_2 <- as.numeric(x = lab_pe$x_2)

  lab_pe$x_2 <- floor(x = lab_pe$x_2)

  lab_pe$x_2 <- as.Date(x = lab_pe$x_2,
                        origin = "1899-12-30")

  names(x = lab_pe)[names(x = lab_pe) == "x_2"] <- "result_date"

# result_modifier

  names(x = lab_pe)[names(x = lab_pe) == "x_3"] <- "result_modifier"

# result_value

  names(x = lab_pe)[names(x = lab_pe) == "x_4"] <- "result_value"

  dplyr::select(.data = lab_pe,
                mrn,
                lab_name,
                result_date,
                result_modifier,
                result_value)

}