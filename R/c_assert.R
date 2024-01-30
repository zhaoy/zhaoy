#' @title Check assertions about data
#'
#' @description
#' Check assertions about data.
#'
#' @usage
#' c_assert(data, predicate, source, ...)
#'
#' @param data A data frame.
#' @param predicate A function that returns FALSE when violated.
#' @param source Logical: TRUE or FALSE.
#' @param ... Comma-separated list of unquoted columns.
#'
#' @return
#' If the predicate assertion is true, TRUE.
#'
#' If the predicate assertion is false and source is TRUE, a data-frame with the offending rows.
#'
#' If the predicate assertion is false and source is FALSE, an assertr data-frame.
#'
#' @importFrom assertr assert error_df_return success_logical
#' @importFrom dplyr filter row_number select
#'
#' @export

c_assert <- function(data,
                     predicate,
                     source,
                     ...) {

  c_assert <- assertr::assert(data = data,
                              predicate = predicate,
                              ...,
                              success_fun = assertr::success_logical,
                              error_fun = assertr::error_df_return)

  if (is.logical(x = c_assert) == TRUE) {

    TRUE

  } else if (is.data.frame(x = c_assert) == TRUE) {

    if (source == TRUE) {

      dplyr::filter(.data = data,
                    dplyr::row_number() %in% c_assert$index == TRUE)

    } else if (source == FALSE) {

      dplyr::select(.data = c_assert,
                    column,
                    index,
                    value)

      }

    }

}