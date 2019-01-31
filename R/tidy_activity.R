#' Tidy Provide Enterprise activity data.
#'
#' @description
#' Tidy the .xlsx version of the Provide Enterprise "Activity Summary by Provider by Client by Date" report.
#'
#' @usage
#' tidy_activity(x)
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

tidy_activity <- function(x) {

  names(x = activity) <- c("x_1",
                           "x_2",
                           "x_3",
                           "x_4",
                           "x_5",
                           "x_6")

  x_1_pattern <- "(musc - adult hiv clinic)|count:|totals|(print date:)"

  activity <- dplyr::filter(.data = activity,
                            grepl(pattern = x_1_pattern,
                                  x = activity$x_1,
                                  ignore.case = TRUE,
                                  fixed = FALSE) == FALSE)

  activity <- dplyr::select(.data = activity,
                            -c(x_4,
                               x_6))

# provider_name

  activity$provider_name <- dplyr::case_when(grepl(pattern = "provider:",
                                                   x = activity$x_1,
                                                   ignore.case = TRUE,
                                                   fixed = FALSE) == TRUE ~
                                             activity$x_1,
                                             grepl(pattern = "provider:",
                                                   x = activity$x_1,
                                                   ignore.case = TRUE,
                                                   fixed = FALSE) == FALSE ~
                                             NA_character_)

  activity$provider_name[is.na(x = activity$provider_name) == FALSE] <- strsplit(x = activity$provider_name[is.na(x = activity$provider_name) == FALSE],
                                                                                 split = "\\s{2}",
                                                                                 fixed = FALSE)

  activity$provider_name[is.na(x = activity$provider_name) == FALSE] <- purrr::map(.x = activity$provider_name[is.na(x = activity$provider_name) == FALSE],
                                                                                   .f = 2)

  activity$provider_name <- c(activity$provider_name,
                              recursive = TRUE)

  activity <- tidyr::fill(data = activity,
                          provider_name,
                          .direction = "down")

  activity <- dplyr::filter(.data = activity,
                            grepl(pattern = "provider:",
                                  x = activity$x_1,
                                  ignore.case = TRUE,
                                  fixed = TRUE) == FALSE)

# activity_type

  activity_type <- unique(x = activity$x_1[activity$x_2 == "service type"],
                          incomparables = FALSE)

  activity$activity_type <- dplyr::case_when(activity$x_1 %in% activity_type == TRUE ~
                                             activity$x_1,
                                             activity$x_1 %in% activity_type == FALSE ~
                                             NA_character_)

  activity <- tidyr::fill(data = activity,
                          activity_type,
                          .direction = "down")

  activity <- dplyr::filter(.data = activity,
                            x_2 != "service type")

# mrn

  activity$mrn <- dplyr::case_when(activity$x_2 == "client id:" ~
                                   activity$x_3,
                                   activity$x_2 != "client id:" ~
                                   NA_character_)

  activity <- tidyr::fill(data = activity,
                          mrn,
                         .direction = "down")

  activity$mrn <- zhaoy::lz_id(x = activity$mrn,
                               lz = TRUE)

# provider_ship

  activity$provider_ship <- dplyr::case_when(activity$x_2 == "client id:" ~
                                             activity$x_5,
                                             activity$x_2 != "client id:" ~
                                             NA_character_)

  activity <- tidyr::fill(data = activity,
                          provider_ship,
                          .direction = "down")

  activity <- dplyr::filter(.data = activity,
                            x_2 != "client id:")

  activity <- dplyr::select(.data = activity,
                            -x_3)

# activity_desc

  names(x = activity)[names(x = activity) == "x_1"] <- "activity_desc"

# activity_date

  activity$x_2 <- as.numeric(x = activity$x_2)

  activity$x_2 <- trunc(x = activity$x_2)

  activity$x_2 <- as.Date(x = activity$x_2,
                          origin = "1899-12-30")

  names(x = activity)[names(x = activity) == "x_2"] <- "activity_date"

  dplyr::select(.data = activity,
                mrn,
                provider_name,
                activity_type,
                activity_desc,
                activity_date,
                provider_ship)

}