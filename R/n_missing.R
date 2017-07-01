#' n_missing
#'
#' n_missing
#'
#' n_missing
#'
#' @param x something
#'
#' @return n_missing
#'
#' @examples
#'
#' n_missing(x = mtcars)
#'
#' @export

n_missing <- function(x) {

    sum_missing <- apply(X = x,
                         MARGIN = 2,
                         FUN = function(x) (sum(is.na(x = x),
                                                na.rm = FALSE)))

    condition <- lapply(X = sum_missing,
                        FUN = function(x) x != 0)

    sum_missing[unlist(x = condition) == TRUE]

}