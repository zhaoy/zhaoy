#' @import purrr

match_med <- function(ref,
                      brand,
                      generic,
                      med) {

  brand <- ref[, brand]

  generic <- ref[, generic]

  ref <- data.frame(brand,
                    generic,
                    row.names = NULL,
                    check.rows = TRUE,
                    check.names = TRUE,
                    fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)

  ref$generic <- ref$generic %>%
    gsub(pattern = "[+-/()]",
         replace = " ") %>%
    strsplit(split = "\\s") %>%
    map(.f = grep,
        pattern = "\\w",
        value = TRUE) %>%
    map(.f = sort,
        decreasing = FALSE,
        na.last = FALSE)

  ref$generic_length <- ref$generic %>%
    map(.f = length) %>%
    unlist

  med <- med %>%
    gsub(pattern = "[+-/()]",
         replacement = " ") %>%
    strsplit(split = "\\s") %>%
    map(.f = grep,
        pattern = "\\D",
        value = TRUE) %>%
    map(.f = function(x) x[x %in% c(ref$brand,
                                    unlist(x = ref$generic)) == TRUE]) %>%
    map(.f = sort,
        decreasing = FALSE,
        na.last = FALSE) %>%
    unlist

  match_med <- map_lgl(.x = ref$brand,
                       .f = function(x) all(med %in% x == TRUE,
                                            na.rm = FALSE))

  if (any(match_med == TRUE,
          na.rm = FALSE) == TRUE) {

    match_med <- which(x = match_med == TRUE)

    match_med <- ref$brand[match_med]

  }

  if (any(match_med == TRUE,
          na.rm = FALSE) == FALSE) {

    match_med <- map_lgl(.x = ref$generic,
                         .f = function(x) all(med %in% x == TRUE,
                                              na.rm = FALSE))

    match_med <- which(x = match_med == TRUE)

    if (length(x = match_med) == 1) {

      match_med <- ref$brand[match_med]

    }

    if (length(x = match_med) > 1) {

      match_med_1 <- ref$brand[match_med]

      match_med_2 <- ref$generic_length[ref$brand %in% match_med_1]

      if (1 %in% match_med_2 == TRUE) {

        match_med <- match_med_1[match_med_2 == 1]

      }

      if (1 %in% match_med_2 == FALSE) {

        match_med <- match_med_1[match_med_2 == min(match_med_2,
                                                    na.rm = FALSE)]

      }

    }

  }

  return(value = match_med)

}