% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s_mode.R
\name{s_mode}
\alias{s_mode}
\title{Statistical mode}
\usage{
s_mode(x)
}
\arguments{
\item{x}{A vector.}
}
\value{
A vector.

If the mode is missing-data: \code{NA}.
}
\description{
Calculate the statistical mode.
}
\examples{
zhaoy::s_mode(x = attenu$dist)
}
