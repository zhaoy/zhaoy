#' Tidy PE laboratory data.
#'
#' @description
#' Tidy the Microsoft Excel .xlsx version of the Provide Enterprise "Test Results by Client ID" report.
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
#' @importFrom purrr map
#' @importFrom rprojroot find_root has_dirname
#' @importFrom readxl read_excel
#' @importFrom utils capture.output
#'
#' @export

tidy_pel <- function(folder,
                     path,
                     sheet = NULL,
                     range = NULL) {

  root_path <- rprojroot::find_root(criterion = has_dirname(dirname = folder),
                                    path = ".")

  import_path <- file.path(root_path,
                           path,
                           fsep = "/")

  pe_lab <- readxl::read_excel(path = import_path,
                               sheet = sheet,
                               range = range,
                               col_names = c("test_name",
                                             "mrn",
                                             "result_modifier",
                                             "result"),
                               col_types = NULL,
                               na = "",
                               trim_ws = TRUE,
                               skip = 0,
                               n_max = Inf,
                               guess_max = 100000)

  pe_lab <- subset(x = pe_lab,
                   subset = test_name != "1" &
                            (is.na(x = result) == TRUE |
                             result != "Result") == TRUE)

  missing_mrn <- subset(x = pe_lab,
                        subset = is.na(x = mrn) == FALSE &
                                 mrn == "Client ID:",
                        select = c(test_name,
                                   mrn))

  if (nrow(x = missing_mrn) != 0) {

    stop("Please find the missing MRN(s):\n",
         call. = FALSE,
         paste0(utils::capture.output(missing_mrn),
                collapse = "\n"))

    } else {

    # test_count

      pe_lab$tcl <- grepl(x = pe_lab$test_name,
                          pattern = "Total Tests for this Client:",
                          ignore.case = TRUE)

      pe_lab$test_count <- NA

      pe_lab$test_count[pe_lab$tcl == TRUE] <- gsub(x = pe_lab$test_name[pe_lab$tcl == TRUE],
                                                    pattern = "Total Tests for this Client:   ",
                                                    replacement = "",
                                                    ignore.case = TRUE)

    # test_date

      pe_lab$tdl <- grepl(x = pe_lab$mrn,
                          pattern = "Client ID:",
                          ignore.case = TRUE)

      pe_lab$test_date <- pe_lab$mrn

      pe_lab$test_date[pe_lab$tdl == TRUE] <- NA

      pe_lab$test_date <- as.numeric(x = pe_lab$test_date)

      pe_lab$test_date <- as.Date(x = as.numeric(x = pe_lab$test_date),
                                  origin = "1899-12-30",
                                  tz = "")

    # mrn: value of 1 MRN is "confidential"

      pe_lab$mrnl <- grepl(x = pe_lab$mrn,
                           pattern = "Client ID:",
                           ignore.case = TRUE)

      pe_lab$mrn_2 <- NA

      pe_lab$mrn_2[pe_lab$mrnl == TRUE] <- gsub(x = pe_lab$mrn[pe_lab$mrnl == TRUE],
                                                pattern = "Client ID:  ",
                                                replacement = "",
                                                ignore.case = TRUE)

      pe_lab$mrn <- NULL

      names(x = pe_lab)[names(x = pe_lab) == "mrn_2"] <- "mrn"

    # test_name

      pe_lab$tnl_1 <- grepl(x = pe_lab$test_name,
                            pattern = " , ",
                            ignore.case = TRUE)

      pe_lab$tnl_2 <- grepl(x = pe_lab$test_name,
                            pattern = "Total Tests for this Client:",
                            ignore.case = TRUE)

      pe_lab$test_name_2 <- pe_lab$test_name

      pe_lab$test_name_2[pe_lab$tnl_1 == TRUE |
                         pe_lab$tnl_2 == TRUE] <- NA

      pe_lab$test_name <- NULL

      names(x = pe_lab)[names(x = pe_lab) == "test_name_2"] <- "test_name"

    # pe_lab: some records are missing result modifiers

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

      pel_test_count <- gsub(x = pel_test_count,
                             pattern = ",",
                             replacement = "",
                             ignore.case = TRUE)

      pel_test_count <- as.integer(x = pel_test_count)

      pel_mrn <- as.character(x = pel_mrn)

      pel_mrn_rep <- rep(x = pel_mrn,
                         times = pel_test_count)

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

      pe_lab <- map(.x = pe_lab,
                    .f = zhaoy_tolower)

      pe_lab <- as.data.frame(x = pe_lab,
                              row.names = NULL,
                              stringsAsFactors = FALSE,
                              cut.names = TRUE,
                              fix.empty.names = TRUE)

      return(value = pe_lab)

    }

}