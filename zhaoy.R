library(package = devtools)
library(package = rprojroot)
library(package = usethis)

path_function <- function(basename,
                          ...) {
  
  criterion <- rprojroot::has_basename(basename = basename)
  
  rprojroot::find_root_file(...,
                            criterion = criterion,
                            path = ".")
  
}

basename <- "zhaoy"

relative_path <- "data/reference/look_up.xlsx"

absolute_path <- path_function(basename = basename,
                               relative_path)

look_up <- readxl::read_excel(path = absolute_path,
                              sheet = "Sheet1",
                              range = NULL,
                              col_names = TRUE,
                              col_types = "text",
                              na = "",
                              trim_ws = TRUE,
                              skip = 0,
                              n_max = Inf,
                              guess_max = 0,
                              progress = FALSE,
                              .name_repair = "minimal")

relative_path <- "code/zhaoy"

dir <- path_function(basename = basename,
                     relative_path)

setwd(dir = dir)

usethis::use_data(look_up,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress = "bzip2",
                  version = 3,
                  ascii = FALSE)

devtools::check()

# devtools::load_all()

devtools::build(pkg = ".",
                path = NULL,
                binary = TRUE,
                vignettes = TRUE,
                manual = TRUE,
                quiet = FALSE)

devtools::install()

# RStudio: Code -> Reflow Comment

# usethis::create_package(path = "zhaoy/code/zhaoy",
#                         rstudio = FALSE,
#                         roxygen = TRUE,
#                         check_name = TRUE,
#                         open = FALSE)

# usethis::use_mit_license()

# DESCRIPTION file's roxygen2 version must match installed version.

# @inheritParams another_package::function.
# To use @inheritParams, the parent function's .R file must exclude @noRd.

# usethis::rename_files()