zhaoy_mode <- function(x) {

  x_table <- table(x,
                   useNA = "ifany")

  x_max <- max(x_table,
               na.rm = TRUE)

  if (all(x_table == x_max,
          na.rm = FALSE) == TRUE) {

    x <- NA

  } else {

    if(is.numeric(x = x) == TRUE) {

      x <- as.numeric(x = names(x = x_table)[x_table == x_max])

    }

    if (is.numeric(x = x) == FALSE) {

      x <- names(x = x_table)[x_table == x_max]

    }

  }

  if (length(x = x) > 1) {

    x <- "multiple"

  }

  return(value = x)

}