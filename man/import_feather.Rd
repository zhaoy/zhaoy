% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_feather.R
\name{import_feather}
\alias{import_feather}
\title{Import Feather files}
\usage{
import_feather(dirname, rpath)
}
\arguments{
\item{dirname}{A directory above 1) the Feather file and 2) the R file.}

\item{rpath}{Relative to \code{dirname}, path to the Feather file.}
}
\value{
A tibble.
}
\description{
Import Feather files.

Convert upper-case English characters to lower-case.
}
\seealso{
\code{\link{export_feather}}
}
