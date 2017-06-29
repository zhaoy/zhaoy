#' Convert PE lab data into tidy data frames
#'
#' Tidies PE lab data that were exported in xlsx files.
#'
#' @param criterion A criterion.
#' @param file Name and extension of the xlsx file.
#' @param sheet Sheet to read.
#' Either a string (the name of a sheet), or an integer (the position of the sheet).
#'
#' @import readxl rprojroot utils
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
                     (is.na(x = pe_lab$result) == TRUE |
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

    # test_count

    pe_lab$tcl <- grep_l(x = pe_lab$test_name,
                         pattern = "Total Tests for this Client:",
                         ignore.case = TRUE)

    pe_lab$test_count <- NA

    pe_lab$test_count[pe_lab$tcl == TRUE] <- g_sub(x = pe_lab$test_name[pe_lab$tcl == TRUE],
                                                   pattern = "Total Tests for this Client:   ",
                                                   replacement = "",
                                                   ignore.case = TRUE)

    # test_date

    pe_lab$tdl <- grep_l(x = pe_lab$mrn,
                         pattern = "Client ID:",
                         ignore.case = TRUE)

    pe_lab$test_date <- pe_lab$mrn

    pe_lab$test_date[pe_lab$tdl == TRUE] <- NA

    pe_lab$test_date <- od(x = pe_lab$test_date,
                           origin = "1899-12-30")

    # mrn: value of 1 MRN is "confidential"

    pe_lab$mrnl <- grep_l(x = pe_lab$mrn,
                          pattern = "Client ID:",
                          ignore.case = TRUE)

    pe_lab$mrn_2 <- NA

    pe_lab$mrn_2[pe_lab$mrnl == TRUE] <- g_sub(x = pe_lab$mrn[pe_lab$mrnl == TRUE],
                                               pattern = "Client ID:  ",
                                               replacement = "",
                                               ignore.case = TRUE)

    pe_lab$mrn <- NULL

    names(x = pe_lab)[names(x = pe_lab) == "mrn_2"] <- "mrn"

    # test_name

    pe_lab$tnl_1 <- grep_l(x = pe_lab$test_name,
                           pattern = " , ",
                           ignore.case = TRUE)

    pe_lab$tnl_2 <- grep_l(x = pe_lab$test_name,
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

    pel_test_count <- g_sub(x = pel_test_count,
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

    return(value = pe_lab)

    }

}