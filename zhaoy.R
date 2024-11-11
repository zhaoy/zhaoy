library(package = devtools)
library(package = usethis)
library(package = zhaoy)

basename <- "gwep"

# usethis::create_package(path = "gwep/code/gwep",
#                         rstudio = FALSE,
#                         roxygen = TRUE,
#                         check_name = TRUE,
#                         open = FALSE)

dir <- zhaoy::path(basename = basename,
                   "code/zhaoy")

setwd(dir = dir)

# Local roxygen2 version must match DESCRIPTION file's version.

# RStudio: Code -> Reflow Comment

# @inheritParams another_package::function

# usethis::rename_files()

# usethis::use_mit_license()

# usethis::use_data(look_up,
#                   internal = FALSE,
#                   overwrite = TRUE,
#                   compress = "bzip2",
#                   version = 3,
#                   ascii = FALSE)

devtools::check()

# devtools::load_all()

devtools::build(pkg = ".",
                path = NULL,
                binary = TRUE,
                vignettes = TRUE,
                manual = TRUE,
                quiet = FALSE)

devtools::install()