#' @import magrittr purrr

med_match <- function(ref,
                      brand,
                      generic,
                      data,
                      med) {

  brand <- ref[, brand]

  generic <- ref[, generic]

  med <- data[, med]

  generic <- generic %>%
    gsub(pattern = "[+-]",
         replace = " ") %>%
    strsplit(split = "\\s") %>%
    map(.f = grep,
        pattern = "\\w",
        value = TRUE) %>%
    map(.f = sort,
        decreasing = FALSE,
        na.last = FALSE)

  generic_length <- generic %>%
    map(.f = length) %>%
    unlist

  ref <- data.frame(brand,
                    generic,
                    generic_length,
                    row.names = NULL,
                    check.rows = TRUE,
                    check.names = TRUE,
                    fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)

  pattern <- "\\D[^capsule{7, 7}gram{4, 4}mg{2, 2}tab{3, 3}tablet{6, 6}]"

  med <- med %>%
  gsub(pattern = "[+-]",
       replacement = " ") %>%
  strsplit(split = "\\s") %>%
  map(.f = grep,
      pattern = pattern,
      value = TRUE) %>%
  map(.f = sort,
      decreasing = FALSE,
      na.last = FALSE)

  match_med <- map_lgl(.x = ref$generic,
                       .f = function(x) all(med %in% x == TRUE,
                                            na.rm = FALSE) == TRUE)

  match_med <- which(x = match_med == TRUE)

  if (length(x = match_med) == 1) {

    match_med <- brand[match_med]
  }

  if (length(x = match_med) > 1) {

    match_med_1 <- brand[match_med]

    match_med_2 <- generic_length[brand %in% match_med_1]

    if (1 %in% match_med_2 == TRUE) {

      match_med <- match_med_1[match_med_2 == 1]

    }

    if (1 %in% match_med_2 == FALSE) {

      match_med <- match_med_1[match_med_2 == min(match_med_2,
                                                  na.rm = FALSE)]

    }

  }

  return(value = match_med)

}

match_med_list <- list(med = med$med_name,
                       med_length = med$med_name_length)

med$match_med <- pmap(.l = match_med_list,
                      .f = match_med,
                      brand = map$drug,
                      generic = map$generic.name,
                      generic_length = map$generic.name_length) %>%
  unlist