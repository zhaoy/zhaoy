#' Read Provide Enterprise lab data
#'
#' Read lab data that is in a .xlsx file downloaded from Provide Enterprise.
#'
#' @param criterion A criterion.
#' @param file Name and extension of a file.
#' @param sheet Sheet to read.
#'
#' @import readxl rprojroot stringr
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

    missing_mrn <- pe_lab[is.na(x = pe_lab$mrn) == FALSE & pe_lab$mrn == "Client ID:",
                          c("test_name",
                            "mrn")]

    if (nrow(x = missing_mrn) != 0) {

      warning("Find these missing MRNs.",
              noBreaks. = TRUE)

      missing_mrn

    }

    pe_lab <- pe_lab[pe_lab$test_name != "1" &
                     (is.na(x = pe_lab$result == TRUE) |
                      pe_lab$result != "Result"), ]

    pe_lab$test_count <- ifelse(test = str_detect(string = pe_lab$test_name,
                                                  pattern = "Total Tests for this Client:") == TRUE,
                                yes = str_split(string = pe_lab$test_name,
                                                pattern = ":   ",
                                                n = 2,
                                                simplify = FALSE),
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

    pe_lab$test_count <- unlist(x = lapply(X = pe_lab$test_count,
                                           FUN = function(x) x[, 2]),
                                recursive = TRUE,
                                use.names = FALSE)

    pe_lab$mrn <- unlist(x = lapply(X = pe_lab$mrn,
                                    FUN = function(x) x[, 2]),
                         recursive = TRUE,
                         use.names = FALSE)

    pe_lab <- pe_lab[c("mrn",
                       "test_count",
                       "test_name",
                       "test_date",
                       "result_modifier",
                       "result")]

    pe_no_replicate <- pe_lab[(is.na(x = pe_lab$test_name) == FALSE) &
                              (is.na(x = pe_lab$test_date) == FALSE),
                              c("test_name",
                                "test_date",
                                "result_modifier",
                                "result")]

    pe_mrn_replicate <- as.character(x = rep(x = na.exclude(object = pe_lab$mrn),
                                             times = na.exclude(object = pe_lab$test_count)))

    pe_test_count_replicate <- as.integer(x = rep(x = na.exclude(object = pe_lab$test_count),
                                                  times = na.exclude(object = pe_lab$test_count)))

    pe_lab <- data.frame(mrn = pe_mrn_replicate,
                         test_count = pe_test_count_replicate,
                         test_name = pe_no_replicate$test_name,
                         test_date = pe_no_replicate$test_date,
                         result_modifier = pe_no_replicate$result_modifier,
                         result = pe_no_replicate$result,
                         row.names = NULL,
                         check.rows = TRUE,
                         check.names = TRUE,
                         fix.empty.names = TRUE,
                         stringsAsFactors = FALSE)

    export_tsv(x = pe_lab,
               folder = root_path)

}