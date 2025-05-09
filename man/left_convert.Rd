% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/left_convert.R
\name{left_convert}
\alias{left_convert}
\title{Convert raw data to analyzable data via left joins}
\usage{
left_convert(raw_df, raw_var, lu_df, lu_category)
}
\arguments{
\item{raw_df}{Raw data-frame.}

\item{raw_var}{Raw data-frame's variable(s) to convert, data in the
variable(s) must be character vector(s) while using this function.}

\item{lu_df}{Look-up data-frame.}

\item{lu_category}{Look-up data-frame's category.}
}
\value{
A data-frame.
}
\description{
Left-join a raw data-frame and a look-up data-frame.
}
