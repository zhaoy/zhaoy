# RStudio: Code -> Reflow Comment

library(package = devtools)
library(package = usethis)
library(package = zhaoy)

# usethis::create_package(path = "gwep/code/gwep",
#                         rstudio = FALSE,
#                         roxygen = TRUE,
#                         check_name = TRUE,
#                         open = FALSE)

# usethis::use_mit_license()

# DESCRIPTION file's roxygen2 version must match local version.

# @inheritParams another_package::function.
# To use @inheritParams, the parent function's .R file must exclude @noRd.

# usethis::rename_files()

basename <- "gwep"

dir <- zhaoy::path(basename = basename,
                   "code/zhaoy")

setwd(dir = dir)

devtools::check()

# devtools::load_all()

devtools::build(pkg = ".",
                path = NULL,
                binary = TRUE,
                vignettes = TRUE,
                manual = TRUE,
                quiet = FALSE)

devtools::install()