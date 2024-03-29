% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_time.R
\name{date_time}
\alias{date_time}
\title{Parse dates-times.}
\usage{
date_time(x, length, orders)
}
\arguments{
\item{x}{A vector.}

\item{length}{Length.}

\item{orders}{Orders.}
}
\value{
A date-time vector.
}
\description{
Parse dates-times.
}
\examples{
x <- c("2000-1-1", "12345")
date_time(x = x, length = 5, orders = "ymd")
}
