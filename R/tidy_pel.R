#' Tidy Provide Enterprise laboratory data.
#'
#' @description
#' Tidy the .xlsx version of the Provide Enterprise "Test Results by Client with ID" report.
#'
#' @usage
#' tidy_pel(folder, path, sheet = NULL, range = NULL)
#'
#' @param folder any folder above both 1) the .xlsx file and 2) the R file.
#' @param path relative to \code{folder}, path to the .xlsx file.
#' @param sheet sheet to read.
#' @param range a cell range to read from.
#'
#' @return
#' A data-frame.
#'
#' @importFrom purrr map map_chr
#' @importFrom zoo na.locf
#'
#' @export

tidy_pel <- function(folder,
                     path,
                     sheet = NULL,
                     range = NULL) {

  pel <- zhaoy::import_excel(folder = folder,
                             path = path,
                             sheet = sheet,
                             range = range)

  pel[1, 1] <- names(x = pel)[1]

  pel[1, 2] <- names(x = pel)[2]

  names(x = pel) <- c("x_1",
                      "x_2",
                      "x_3",
                      "x_4")

  pel <- subset(x = pel,
                subset = is.na(x = x_1) == FALSE &
                         x_1 != "1" &
                         grepl(pattern = "(test name)|(total tests for this client:)",
                               x = pel$x_1,
                               ignore.case = TRUE,
                               fixed = FALSE) == FALSE)

# mrn

  pel$mrn <- ifelse(test = grepl(pattern = "client id:",
                                 x = pel$x_2,
                                 fixed = TRUE),
                    yes = pel$x_2,
                    no = NA_character_)

  pel$mrn[is.na(x = pel$mrn) == FALSE] <- strsplit(x = pel$mrn[is.na(x = pel$mrn) == FALSE],
                                                   split = "",
                                                   fixed = TRUE)

  pel$mrn[is.na(x = pel$mrn) == FALSE] <- purrr::map(.x = pel$mrn[is.na(x = pel$mrn) == FALSE],
                                                     .f = grep,
                                                     pattern = "\\d",
                                                     value = TRUE,
                                                     fixed = FALSE,
                                                     invert = FALSE)

  pel$mrn[is.na(x = pel$mrn) == FALSE] <- purrr::map_chr(.x = pel$mrn[is.na(x = pel$mrn) == FALSE],
                                                         .f = paste,
                                                         sep = "",
                                                         collapse = "")

  pel$mrn <- zoo::na.locf(object = pel$mrn,
                          na.rm = FALSE,
                          fromLast = FALSE)

  pel <- subset(x = pel,
                subset = is.na(x = mrn) == FALSE &
                         mrn != "")

  pel$mrn <- zhaoy::lz_id(x = pel$mrn,
                          lz = TRUE)

  pel <- subset(x = pel,
                subset = is.na(x = x_2) == FALSE &
                         grepl(pattern = "client id:",
                               x = pel$x_2,
                               fixed = TRUE) == FALSE &
                         (is.na(x = x_3) == TRUE |
                          (is.na(x = x_3) == FALSE &
                           x_3 != "result modifier") == TRUE))

# lab_name

  names(x = pel)[names(x = pel) == "x_1"] <- "lab_name"

# result_date

  names(x = pel)[names(x = pel) == "x_2"] <- "result_date"

  pel$result_date <- as.numeric(x = pel$result_date)

  pel$result_date <- trunc(x = pel$result_date)

  pel$result_date <- as.Date(x = pel$result_date,
                             origin = "1899-12-30")

# result_modifier

  names(x = pel)[names(x = pel) == "x_3"] <- "result_modifier"

# result_value

  names(x = pel)[names(x = pel) == "x_4"] <- "result_value"

  subset(x = pel,
         select = c(mrn,
                    lab_name,
                    result_date,
                    result_modifier,
                    result_value))

}