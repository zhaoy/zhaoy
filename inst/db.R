library(package = DBI)
library(package = dplyr)

con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = "[your driver's name]",
                      Host   = "[your server's path]",
                      SVC    = "[your schema's name]",
                      UID    = rstudioapi::askForPassword("Database user"),
                      PWD    = rstudioapi::askForPassword("Database password"),
                      Port   = 1521)

dbDataType

dbIsValid

dbListTables(conn = con)

dbGetRowCount

dbListFields(conn = con,
             name = NULL)

dbColumnInfo

dbReadTable(conn = con,
            name = NULL)

# use dbQuoteIdentifier(conn = x = ) with dbReadTable

#db <- tbl(src = con,
#          x = NULL) %>%
#collect

airport_code <- "GPT"

sql <- sqlInterpolate(conn = con,
                      sql = "SELECT * FROM airports  where faa = ?code", 
                      code = airport_code)

dbGetQuery(conn = con,
           statement = sql)

dbClearResult(res = db)

dbDisconnect(conn = con)