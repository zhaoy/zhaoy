% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/i_df.R
\name{i_df}
\alias{i_df}
\title{Convert upper-case English characters to lower-case}
\usage{
i_df(x)
}
\arguments{
\item{x}{A data-frame.}
}
\value{
A data-frame.
}
\description{
Convert upper-case English characters to lower-case.
}
\examples{
ToothGrowth$supp <- as.character(x = ToothGrowth$supp)
zhaoy::i_df(x = ToothGrowth)
}
