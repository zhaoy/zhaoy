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

  pea <- zhaoy::import_excel(folder = folder,
                             path = path,
                             sheet = sheet,
                             range = range)

  names(x = pea) <- c("x_1",
                      "x_2",
                      "x_3",
                      "x_4",
                      "x_5",
                      "x_6")

  x_1_pattern <- "(musc - adult hiv clinic)|count:|totals|(print date:)"

  pea <- subset(x = pea,
                subset = grepl(pattern = x_1_pattern,
                               x = pea$x_1,
                               fixed = FALSE) == FALSE,
                select = -c(x_4,
                            x_6))

# provider

  pea$provider <- ifelse(test = grepl(pattern = "provider:",
                                      x = pea$x_1,
                                      fixed = TRUE),
                         yes = pea$x_1,
                         no = NA_character_)

  pea$provider <- zoo::na.locf(object = pea$provider,
                               na.rm = FALSE,
                               fromLast = FALSE)

  pea$provider <- substring(text = pea$provider,
                            first = 12,
                            last = nchar(x = pea$provider,
                                         allowNA = FALSE,
                                         keepNA = TRUE))

  pea <- subset(x = pea,
                subset = grepl(pattern = "provider:",
                               x = pea$x_1,
                               fixed = TRUE) == FALSE)

# activity_type

  activity_type <- unique(x = pea$x_1[pea$x_2 == "service type"],
                          incomparables = FALSE)

  pea$activity_type <- ifelse(test = pea$x_1 %in% activity_type == TRUE,
                              yes = pea$x_1,
                              no = NA_character_)

  pea$activity_type <- zoo::na.locf(object = pea$activity_type,
                                    na.rm = FALSE,
                                    fromLast = FALSE)

  pea <- subset(x = pea,
                subset = pea$x_2 != "service type")

# mrn

  pea$mrn <- ifelse(test = pea$x_2 == "client id:",
                    yes = pea$x_3,
                    no = NA_character_)

  pea$mrn <- zoo::na.locf(object = pea$mrn,
                          na.rm = FALSE,
                          fromLast = FALSE)

  pea$mrn <- zhaoy::lz_id(x = pea$mrn,
                          lz = TRUE)

# provider_ship

  pea$provider_ship <- ifelse(test = pea$x_2 == "client id:",
                              yes = pea$x_5,
                              no = NA_character_)

  pea$provider_ship <- zoo::na.locf(object = pea$provider_ship,
                                    na.rm = FALSE,
                                    fromLast = FALSE)

  pea <- subset(x = pea,
                subset = x_2 != "client id:",
                select = -x_3)

# activity_desc

  names(x = pea)[names(x = pea) == "x_1"] <- "activity_desc"

# activity_date

  names(x = pea)[names(x = pea) == "x_2"] <- "activity_date"

  pea$activity_date <- as.numeric(x = pea$activity_date)

  pea$activity_date <- trunc(x = pea$activity_date)

  pea$activity_date <- as.Date(x = pea$activity_date,
                               origin = "1899-12-30")

  subset(x = pea,
         select = c(mrn,
                    provider,
                    activity_type,
                    activity_desc,
                    activity_date,
                    provider_ship))

}