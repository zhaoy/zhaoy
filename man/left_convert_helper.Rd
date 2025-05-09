% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/left_convert_helper.R
\name{left_convert_helper}
\alias{left_convert_helper}
\title{Convert raw data to analyzable data via left joins}
\usage{
left_convert_helper(raw_df, raw_var, lu_df, lu_category)
}
\arguments{
\item{raw_df}{Raw data-frame.}

\item{raw_var}{Raw data-frame's variable(s) to convert, data in the
variable(s) must be character vector(s) while using this function.}

\item{lu_df}{Look-up data-frame.}

\item{lu_category}{Look-up data-frame's category.}
}
\value{
A character vector.
}
\description{
Left-join a raw data-frame and a look-up data-frame.
}
