# RStudio: Code -> Reflow Comment

library(package = devtools)
library(package = rprojroot)
library(package = usethis)

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

# rpath <- "data/reference/look_up.xlsx"
# 
# look_up <- zhaoy::import_excel(dirname = basename,
#                                rpath = rpath,
#                                sheet = "Sheet1")

path_function <- function(basename,
                          ...) {
  
  criterion <- rprojroot::has_basename(basename = basename)
  
  rprojroot::find_root_file(...,
                            criterion = criterion,
                            path = ".")
  
}

dir <- path_function(basename = basename,
                     "code/zhaoy")

setwd(dir = dir)

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