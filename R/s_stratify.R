#' @title 
#' Summarize data in uni-variate or bivariate tables
#'
#' @description
#' Summarize data in uni-variate or bivariate tables.
#'
#' @usage
#' s_stratify(data, formula = NULL, strata_2 = NULL, min_dis, max_level, ...)
#'
#' @param data Data-frame.
#' @param formula For uni-variate tables: one variable followed by "~ .". For bivariate tables: use "~" to separate dependent and independent variable(s), use "+" to separate independent variables.
#' @param strata_2 Applies only to bivariate tables. Beyond the dependent variable in \code{formula}, additional variable to stratify on. Must be character while using this function.
#' @param min_dis If a non-factor dependent variable has less than \code{min.dis} distinct data-points, then this function treats the variable as categorical.
#' @param max_level Maximum number of distinct data-points of dependent variable, categorical independent variables, and \code{strata_2}.
#' @param ... Additional arguments.
#'
#' @return
#' A uni-variate or bivariate table.
#'
#' @importFrom compareGroups descrTable strataTable
#' 
#' @export

s_stratify <- function(data,
                       formula = NULL,
                       strata_2 = NULL,
                       min_dis,
                       max_level,
                       ...) {
  
  args <- list(...)
  
  args$data <- data
  
  args$formula <- formula
  
  args$min.dis <- min_dis
  
  args$max.xlev <- max_level
  
  args$max.ylev <- max_level
  
  args$include.label <- FALSE
  
  args$method <- NA
  
  args$Q1 <- 0
  
  args$Q3 <- 1
  
  args$simplify <- FALSE
  
  args$digits <- 2
  
  args$show.all <- TRUE
  
  args$show.descr <- TRUE
  
  args$show.n <- TRUE
  
  args$show.p.overall <- FALSE
  
  table <- do.call(what = compareGroups::descrTable,
                   args = args)
  
  if (is.null(x = strata_2)) {
    
    table
    
  } else if (! is.null(x = strata_2)) {
    
    compareGroups::strataTable(x = table,
                               strata = strata_2,
                               strata.names = NULL,
                               max.nlevels = max_level)
    
  }

}