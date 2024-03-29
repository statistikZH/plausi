% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_funs.R
\name{DoubleMADsFromMedian}
\alias{DoubleMADsFromMedian}
\title{Double Mads from Median}
\usage{
DoubleMADsFromMedian(x, zero.mad.action = "warn")
}
\arguments{
\item{x}{vector of numeric values}

\item{zero.mad.action}{action in the event of a MAD of zero (Options: "stop", "warn", "na" and "warn and na")}
}
\value{
numeric vector
}
\description{
Find Outliers in terms of their distance in terms of MADs from the Median for the values of a distributions.
}
\examples{
x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)

DoubleMADsFromMedian(x)
}
