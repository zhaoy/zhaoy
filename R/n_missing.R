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

    n_missing <- apply(X = x,
                       MARGIN = 2,
                       FUN = function(x) (sum(is.na(x = x),
                                              na.rm = FALSE)))

    condition <- lapply(X = n_missing,
                        FUN = function(x) x != 0)

    n_missing <- n_missing[unlist(x = condition) == TRUE]

    n_missing <- data.frame(n_missing)

    n_missing$variable <- rownames(x = n_missing)

    rownames(x = n_missing) <- NULL

    n_missing <- n_missing[, c("variable",
                               "n_missing")]

    return(value = n_missing)

}