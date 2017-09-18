library(package = DBI)

con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = "[your driver's name]",
                      Host = "[your server's path]",
                      SVC = "[your schema's name]",
                      UID = rstudioapi::askForPassword("Database user"),
                      PWD = rstudioapi::askForPassword("Database password"),
                      Port = 1521)

# is object valid

dbIsValid(dbObj = con)

# list table(s)

dbListTables(conn = con)

# individual table(s)

# data type of object

dbDataType(dbObj = con,
           obj = "table")

# names of table

dbListFields(conn = con,
             name = "table")

# interpolate query

sql <- "SELECT * FROM table where faa = ?code"

code <- "code"

statement <- sqlInterpolate(conn = con,
                            sql = sql, 
                            code = code)

# send query

db <- dbGetQuery(conn = con,
                 statement = statement)

dbClearResult(res = db)

dbDisconnect(conn = con)