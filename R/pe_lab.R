#' Convert PE lab data into tidy data frames
#'
#' Tidies PE lab data that were exported in xlsx files.
#'
#' @param criterion A criterion.
#' @param file Name and extension of the xlsx file.
#' @param sheet Sheet to read.
#' Either a string (the name of a sheet), or an integer (the position of the sheet).
#'
#' @import readxl rprojroot stringr utils
#'
#' @export
#'
#' @examples

pe_lab <- function(criterion,
                   file,
                   sheet) {

    root_path <- find_root(criterion = criterion,
                           path = ".")

    import_path <- file.path(root_path,
                             file,
                             fsep = "/")

    pe_lab <- read_excel(path = import_path,
                         sheet = sheet,
                         range = NULL,
                         col_names = c("test_name",
                                       "mrn",
                                       "result_modifier",
                                       "result"),
                         col_types = NULL,
                         na = "",
                         trim_ws = TRUE,
                         skip = 0,
                         n_max = Inf,
                         guess_max = 10)

    pe_lab <- pe_lab[pe_lab$test_name != "1" &
                     (is.na(x = pe_lab$result == TRUE) |
                      pe_lab$result != "Result"), ]

    missing_mrn <- pe_lab[is.na(x = pe_lab$mrn) == FALSE &
                          pe_lab$mrn == "Client ID:",
                          c("test_name",
                            "mrn")]

    if (nrow(x = missing_mrn) != 0) {

      stop("Please find the missing MRN(s):\n",
           call. = FALSE,
           paste0(capture.output(missing_mrn),
                  collapse = "\n"))

    }

    else {

    pe_lab$test_count <- ifelse(test = grepl(x = pe_lab$test_name,
                                             pattern = "Total Tests for this Client:",
                                             fixed = TRUE) == TRUE,
                                yes = strsplit(x = pe_lab$test_name,
                                               split = ":   ",
                                               fixed = TRUE),
                                no = NA)

    pe_lab$test_date <- ifelse(test = str_detect(string = pe_lab$mrn,
                                                 pattern = "Client ID:") == TRUE,
                               yes = NA,
                               no = pe_lab$mrn)

    pe_lab$test_date <- od(x = pe_lab$test_date,
                           origin = "1899-12-30")

    pe_lab$mrn_2 <- ifelse(test = str_detect(string = pe_lab$mrn,
                                             pattern = "Client ID:") == TRUE,
                           yes = str_split(string = pe_lab$mrn,
                                           pattern = ":  ",
                                           n = 2,
                                           simplify = FALSE),
                           no = NA)

    pe_lab$mrn <- NULL

    names(x = pe_lab)[names(x = pe_lab) == "mrn_2"] <- "mrn"

    pe_lab$test_name_2 <- ifelse(test = (str_detect(string = pe_lab$test_name,
                                                    pattern = " , ") |
                                         str_detect(string = pe_lab$test_name,
                                                    pattern = "Total Tests for this Client:")) == TRUE,
                                 yes = NA,
                                 no = pe_lab$test_name)

    pe_lab$test_name <- NULL

    names(x = pe_lab)[names(x = pe_lab) == "test_name_2"] <- "test_name"

    pe_lab[, c("test_count",
               "mrn")] <- apply(X = pe_lab[, c("test_count",
                                               "mrn")],
                                MARGIN = 2,
                                FUN = function(x) unlist(x = lapply(X = x,
                                                                    FUN = function(x) x[2])))

    pe_lab <- pe_lab[, c("mrn",
                         "test_count",
                         "test_name",
                         "test_date",
                         "result_modifier",
                         "result")]

    pel_no_rep <- pe_lab[(is.na(x = pe_lab$test_name) == FALSE) &
                         (is.na(x = pe_lab$test_date) == FALSE),
                         c("test_name",
                           "test_date",
                           "result_modifier",
                           "result")]

    pel_mrn <- pe_lab$mrn[is.na(x = pe_lab$mrn) == FALSE]

    pel_test_count <- pe_lab$test_count[is.na(x = pe_lab$test_count) == FALSE]

    pel_test_count <- as.integer(x = gsub(pattern = ",",
                                          replacement = "",
                                          x = pel_test_count,
                                          ignore.case = TRUE))

    pel_mrn_rep <- as.character(x = rep(x = pel_mrn,
                                        times = pel_test_count))

    pel_test_count_rep <- rep(x = pel_test_count,
                              times = pel_test_count)

    pe_lab <- data.frame(mrn = pel_mrn_rep,
                         test_count = pel_test_count_rep,
                         test_name = pel_no_rep$test_name,
                         test_date = pel_no_rep$test_date,
                         result_modifier = pel_no_rep$result_modifier,
                         result = pel_no_rep$result,
                         row.names = NULL,
                         check.rows = TRUE,
                         check.names = TRUE,
                         fix.empty.names = TRUE,
                         stringsAsFactors = FALSE)

    return(value = pe_lab)

    }

}