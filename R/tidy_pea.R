#' Tidy Provide Enterprise activity data.
#'
#' @description
#' Tidy the .xlsx version of the Provide Enterprise "Activity Summary by Provider by Client by Date" report.
#'
#' @usage
#' tidy_pea(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder any folder above both 1) the .xlsx file and 2) the R file.
#' @param path relative to \code{folder}, path to the .xlsx file.
#' @param sheet sheet to read.
#' @param range a cell range to read from.
#'
#' @return
#' A data-frame.
#'
#' @importFrom zoo na.locf
#'
#' @export

tidy_pea <- function(folder,
                     path,
                     sheet = NULL,
                     range = NULL) {

  activity <- zhaoy::import_excel(folder = folder,
                                  path = path,
                                  sheet = sheet,
                                  range = range)

  names(x = activity) <- c("x_1",
                           "x_2",
                           "x_3",
                           "x_4",
                           "x_5",
                           "x_6")

  activity_x_1_pattern <- "(musc - adult hiv clinic)|count:|totals|(print date:)"

  activity <- subset(x = activity,
                     subset = grepl(pattern = activity_x_1_pattern,
                                    x = activity$x_1,
                                    fixed = FALSE) == FALSE,
                     select = -c(x_4,
                                 x_5,
                                 x_6))

# provider

  activity$provider <- ifelse(test = grepl(pattern = "provider:",
                                           x = activity$x_1,
                                           fixed = TRUE),
                              yes = activity$x_1,
                              no = NA_character_)

  activity$provider <- zoo::na.locf(object = activity$provider,
                                    na.rm = FALSE,
                                    fromLast = FALSE)

  activity$provider <- substring(text = activity$provider,
                                 first = 11,
                                 last = nchar(x = activity$provider,
                                              allowNA = FALSE,
                                              keepNA = TRUE))

  activity <- subset(x = activity,
                     subset = grepl(pattern = "provider:",
                                    x = x_1,
                                    fixed = TRUE) == FALSE)

# activity_type

  activity_type <- unique(x = activity$x_1[activity$x_2 == "service type"],
                          incomparables = FALSE)

  activity$activity_type <- ifelse(test = activity$x_1 %in% activity_type == TRUE,
                                   yes = activity$x_1,
                                   no = NA_character_)

  activity$activity_type <- zoo::na.locf(object = activity$activity_type,
                                         na.rm = FALSE,
                                         fromLast = FALSE)

  activity <- subset(x = activity,
                     subset = activity$x_2 != "service type")

# mrn

  activity$mrn <- ifelse(test = activity$x_2 == "client id:",
                         yes = activity$x_3,
                         no = NA_character_)

  activity$mrn <- zoo::na.locf(object = activity$mrn,
                               na.rm = FALSE,
                               fromLast = FALSE)

  activity$mrn <- zhaoy::lz_id(x = activity$mrn,
                               lz = TRUE)

  activity <- subset(x = activity,
                     subset = x_2 != "client id:",
                     select = -x_3)

# activity_desc

  names(x = activity)[names(x = activity) == "x_1"] <- "activity_desc"

# activity_date

  names(x = activity)[names(x = activity) == "x_2"] <- "activity_date"

  activity$activity_date <- as.numeric(x = activity$activity_date)

  activity$activity_date <- trunc(x = activity$activity_date)

  activity$activity_date <- as.Date(x = activity$activity_date,
                                    origin = "1899-12-30")

  activity <- subset(x = activity,
                     select = c(mrn,
                                provider,
                                activity_type,
                                activity_desc,
                                activity_date))

  return(value = activity)

}