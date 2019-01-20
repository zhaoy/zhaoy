#' Tidy Provide Enterprise activity data.
#'
#' @description
#' Tidy the .xlsx version of the Provide Enterprise "Activity Summary by Provider by Client by Date" report.
#'
#' @usage
#' tidy_pea(x)
#'
#' @param x a data-frame.
#'
#' @return
#' A data-frame.
#'
#' @importFrom tidyr fill
#'
#' @export

tidy_pea <- function(x) {

  pea <- x

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

# provider_name

  pea$provider_name <- ifelse(test = grepl(pattern = "provider:",
                                           x = pea$x_1,
                                           fixed = TRUE),
                              yes = pea$x_1,
                              no = NA_character_)

  pea$provider_name <- tidyr::fill(data = pea,
                                   provider_name,
                                   .direction = "down")

  pea$provider_name <- substring(text = pea$provider_name,
                                 first = 12,
                                 last = nchar(x = pea$provider_name,
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

  pea$activity_type <- tidyr::fill(data = pea,
                                   activity_type,
                                   .direction = "down")

  pea <- subset(x = pea,
                subset = pea$x_2 != "service type")

# mrn

  pea$mrn <- ifelse(test = pea$x_2 == "client id:",
                    yes = pea$x_3,
                    no = NA_character_)

  pea$mrn <- tidyr::fill(data = pea,
                         mrn,
                         .direction = "down")

  pea$mrn <- zhaoy::lz_id(x = pea$mrn,
                          lz = TRUE)

# provider_ship

  pea$provider_ship <- ifelse(test = pea$x_2 == "client id:",
                              yes = pea$x_5,
                              no = NA_character_)

  pea$provider_ship <- tidyr::fill(data = pea,
                                   provider_ship,
                                   .direction = "down")

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
                    provider_name,
                    activity_type,
                    activity_desc,
                    activity_date,
                    provider_ship))

}