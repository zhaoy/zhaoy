#' Tidy xovide Enterxise provider-relationship data.
#'
#' @description
#' Tidy the xovide Enterxise "Case Loads by provider Relationship" report.
#'
#' @usage
#' tidy_provider_relationship(x)
#'
#' @param x A data-frame.
#'
#' @return
#' A tibble.
#'
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr fill
#'
#' @export

tidy_provider_relationship <- function(x) {
  
  # Rename certain variables.
  
  x <- dplyr::rename(.data = x,
                     mrn = organization.program...access.network.inc....case.management,
                     admin = ...2)
  
  # In the "mrn" variable, include only ID-physician or MRN data:
  
  x <- dplyr::filter(.data = x,
                      grepl(pattern = "(i.d. physician -  )|(^((\\d)+)$)",
                            x = mrn,
                            ignore.case = TRUE,
                            fixed = FALSE) == TRUE)
  
  # -In ID-physician data, include only ID-physician-name data.
  
  x$mrn <- gsub(pattern = "i.d. physician -  ",
                replacement = "",
                x = x$mrn,
                ignore.case = TRUE,
                fixed = FALSE)
  
  # -In MRN data, convert data to 9-digit strings.
  
  x$mrn[is.na(x = x$mrn) == FALSE &
        grepl(pattern = "(\\d)+",
              x = x$mrn,
              fixed = FALSE) == TRUE] <- x$mrn %>%
    subset(subset = is.na(x = x$mrn) == FALSE &
           grepl(pattern = "(\\d)+",
                 x = x$mrn,
                 fixed = FALSE) == TRUE) %>%
    zhaoy::lz_id(lz = TRUE)
  
  # Create the "provider_name" variable.
  
  x <- dplyr::mutate(.data = x,
                     provider_name = NA)
  
  x$provider_name[is.na(x = x$mrn) == FALSE &
                  grepl(pattern = "[a-z]+",
                        x = x$mrn,
                        ignore.case = TRUE,
                        fixed = FALSE) == TRUE] <- x$mrn %>%
    subset(subset = is.na(x = x$mrn) == FALSE &
           grepl(pattern = "[a-z]+",
                 x = x$mrn,
                 ignore.case = TRUE,
                 fixed = FALSE) == TRUE)
  
  # In the "provider_name" variable, fill in missing data.
  
  x <- tidyr::fill(data = x,
                   provider_name,
                   .direction = "down")
  
  # In the "admin" variable, include only "open" data.
  
  x <- dplyr::filter(.data = x,
                     admin == "open")
  
  # Select variables.
  
  dplyr::select(.data = x,
                mrn,
                provider_name)
  
}