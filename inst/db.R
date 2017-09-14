library(package = dplyr)
library(package = DBI)
con <- DBI::dbConnect(drv = odbc::odbc(),
                      path = NULL,
                      user = "hadley",
                      password = rstudioapi::askForPassword("db"),
                      host = "database.rstudio.com",
                      port = NULL,
                      dbname = NULL)

db <- tbl(src = con,
          x = something)

db <- collect(x = db)