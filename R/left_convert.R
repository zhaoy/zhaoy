#' @title Convert data via left join
#'
#' @description
#' Convert data via left join.
#'
#' @usage
#' left_convert(category, x_df, x_var, y_df, y_var)
#'
#' @param category Category.
#' @param x_df Left-hand-side data-frame.
#' @param x_var Left-hand-side variable.
#' @param y_df Left-hand-side data-frame.
#' @param y_var Left-hand-side variable.
#'
#' @return
#' A tibble.
#'
#' @importFrom dplyr join_by left_join select
#'
#' @export

left_convert <- function(category,
                         x_df,
                         x_var,
                         y_df,
                         y_var) {

  y_df <- y_df[! is.na(x = y_df$category) &
               y_df$category == category, ]

  x_df <- dplyr::left_join(x = x_df,
                           y = y_df,
                           by = dplyr::join_by({{ x_var }} == {{ y_var }}),
                           keep = NULL,
                           multiple = "all",
                           unmatched = "drop",
                           relationship = "many-to-one")

  x_df[[x_var]][! is.na(x = x_df$analyzable)] <- x_df$analyzable[! is.na(x = x_df$analyzable)]

  x_df[[x_var]][! is.na(x = x_df$analyzable) &
                x_df$analyzable == "convert to missing"] <- NA

  dplyr::select(.data = x_df,
                ! c(category,
                    analyzable))

}