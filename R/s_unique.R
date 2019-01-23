#' Unique Values
#'
#' @description
#' Tabulate counts and percents of unique values, including missing-data.
#'
#' @usage
#' s_unique(x)
#'
#' @param x a vector.
#'
#' @return
#' A tibble.
#'
#' @seealso \code{\link{s_mode} \link{s_s}}
#'
#' @importFrom dplyr arrange combine
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' s_unique(x = attenu$station)

s_unique <- function(x) {

  options(tibble.print_max = Inf,
          tibble.width = Inf)

  stopifnot(inherits(x = x,
                     what = dplyr::combine("character",
                                           "integer",
                                           "logical",
                                           "numeric",
                                           "factor",
                                           "Date",
                                           "difftime",
                                           "POSIXct",
                                           "POSIXlt"),
                     which = FALSE),
            is.list(x = x) == FALSE)

  ciln <- inherits(x = x,
                   what = dplyr::combine("character",
                                         "integer",
                                         "logical",
                                         "numeric"),
                   which = FALSE)

  count <- table(x,
                 useNA = "ifany")

  value <- names(x = count)

  if (ciln == TRUE) {

    class(x = value) <- class(x = x)

    mode(x = value) <- mode(x = x)

  }

  pct <- prop.table(x = count) * 100

  pct <- round(x = pct,
               digits = 1)

  s_unique <- tibble::tibble(value,
                             count,
                             pct,
                             .rows = NULL,
                             .name_repair = "universal")

  s_unique$count <- as.integer(x = s_unique$count)

  s_unique$pct <- as.numeric(x = s_unique$pct)

  dplyr::arrange(.data = s_unique,
                 value)

}