ss_stratify <- function(data,
                        formula = NULL,
                        strata_2 = NULL,
                        max_dis,
                        level) {
  
  group <- compareGroups::compareGroups(data = data,
                                        formula = formula,
                                        include.label = FALSE,
                                        min.dis = max_dis,
                                        max.xlev = level,
                                        max.ylev = level,
                                        method = 2,
                                        Q1 = 0,
                                        Q3 = 1,
                                        simplify = FALSE)
  
  table <- compareGroups::createTable(x = group,
                                      digits = 2,
                                      show.all = TRUE,
                                      show.descr = TRUE,
                                      show.n = TRUE,
                                      show.p.overall = FALSE)
  
  if (is.null(x = strata)) {
    
    table
    
  } else if (! is.null(x = strata)) {
    
    compareGroups::strataTable(x = table,
                               strata = strata_2,
                               strata.names = NULL,
                               max.nlevels = level)
    
    }
  
}