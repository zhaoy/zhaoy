#' @title Clean some parts of data-frames
#'
#' @description Clean variable-names and data-point white-space.
#'
#' @usage clean_df(data)
#'
#' @param data Data-frame.
#'
#' @returns A data-frame.
#'
#' @importFrom dplyr across mutate
#' @importFrom janitor clean_names
#' @importFrom stringr str_squish
#' @importFrom tidyselect where
#'
#' @export

clean_df <- function(data) {

  data <- janitor::clean_names(dat = data,
                               case = "snake",
                               ascii = TRUE,
                               use_make_names = TRUE,
                               allow_dupes = FALSE,
                               numerals = "asis")

  dplyr::mutate(.data = data,
                dplyr::across(.cols = tidyselect::where(fn = is.character),
                              .fns = stringr::str_squish,
                              .names = "{.col}"),
                .keep = "all")

}