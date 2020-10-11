#' Tidy Provide Enterprise activity data.
#'
#' @description
#' Tidy the Provide Enterprise "Activity Summary By Provider by Client by Date" report.
#'
#' @usage
#' tidy_activity(x)
#'
#' @param x A data-frame.
#'
#' @return
#' A tibble.
#'
#' @importFrom dplyr case_when filter mutate select
#' @importFrom lubridate as_date
#' @importFrom purrr map_chr
#' @importFrom tidyr fill
#'
#' @export

tidy_activity <- function(x) {

  activity <- x
  
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
                            x_1,
                            x_2,
                            x_3,
                            x_5)
  
  # activity_person
  
  activity <- dplyr::mutate(.data = activity,
                            activity_person = dplyr::case_when(grepl(pattern = "provider:",
                                                                     x = activity$x_1,
                                                                     ignore.case = TRUE,
                                                                     fixed = FALSE) == TRUE ~
                                                               activity$x_1,
                                                               grepl(pattern = "provider:",
                                                                     x = activity$x_1,
                                                                     ignore.case = TRUE,
                                                                     fixed = FALSE) == FALSE ~
                                                               NA_character_))
  
  activity$activity_person[is.na(x = activity$activity_person) == FALSE] <- activity$activity_person %>%
    subset(subset = is.na(x = activity$activity_person) == FALSE) %>%
    strsplit(split = ":\\s{2}",
             fixed = FALSE) %>%
    purrr::map_chr(.f = 2)
  
  activity$activity_person <- gsub(pattern = "\\s{2,}",
                                   replacement = " ",
                                   x = activity$activity_person,
                                   ignore.case = TRUE,
                                   fixed = FALSE)
  
  activity$activity_person <- gsub(pattern = "/musc/scgov",
                                   replacement = "",
                                   x = activity$activity_person,
                                   ignore.case = TRUE,
                                   fixed = FALSE)
  
  activity <- tidyr::fill(data = activity,
                          activity_person,
                          .direction = "down")
  
  activity <- dplyr::filter(.data = activity,
                            grepl(pattern = "provider:",
                                  x = activity$x_1,
                                  ignore.case = TRUE,
                                  fixed = FALSE) == FALSE)
  
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
  
  activity <- dplyr::filter(.data = activity,
                            x_2 != "client id:")
  
  activity <- dplyr::select(.data = activity,
                            x_1,
                            x_2,
                            activity_person,
                            mrn)
  
  # activity_type
  
  x_1_pattern <- unique(x = activity$x_1[activity$x_2 == "service type"],
                        incomparables = FALSE)
  
  activity$activity_type <- dplyr::case_when(activity$x_1 %in% x_1_pattern == TRUE ~
                                             activity$x_1,
                                             activity$x_1 %in% x_1_pattern == FALSE ~
                                             NA_character_)
  
  activity <- tidyr::fill(data = activity,
                          activity_type,
                          .direction = "down")
  
  activity <- dplyr::filter(.data = activity,
                            x_2 != "service type")
  
  # activity_date
  
  activity$x_2 <- as.numeric(x = activity$x_2)
  
  activity$x_2 <- trunc(x = activity$x_2)
  
  activity$x_2 <- lubridate::as_date(x = activity$x_2,
                                     origin = "1899-12-30")
  
  # activity
  
  dplyr::select(.data = activity,
                mrn,
                activity_date = x_2,
                activity_person,
                activity_type,
                activity_description = x_1)

}