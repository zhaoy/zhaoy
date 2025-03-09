#' @title Clean data-frames
#'
#' @description Clean variable-names, white-space, and capitalization.
#'
#' @usage clean_df(data, lc_var)
#'
#' @param data Data-frame.
#' @param lc_var Data-frame's variable(s) to lower-case, data in the
#'   variable(s) must be character vector(s) while using this function.
#'
#' @returns A data-frame.
#'
#' @importFrom dplyr across mutate
#' @importFrom janitor clean_names
#' @importFrom stringr str_squish str_to_lower
#' @importFrom tidyselect where
#'
#' @export

clean_df <- function(data,
                     lc_var = NULL) {

  data <- janitor::clean_names(dat = data,
                               case = "snake",
                               ascii = TRUE,
                               use_make_names = TRUE,
                               allow_dupes = FALSE,
                               numerals = "asis")

  data <- dplyr::mutate(.data = data,
                        dplyr::across(.cols = tidyselect::where(fn = is.character),
                                      .fns = stringr::str_squish,
                                      .names = "{.col}"),
                        .keep = "all")

  data <- dplyr::mutate(.data = data,
                        dplyr::across(.cols = {{ lc_var }},
                                      .fns = function(x)
                                             stringr::str_to_lower(string = x,
                                                                   locale = "en"),
                                      .names = "{.col}"),
                        .keep = "all")
  
  data

}