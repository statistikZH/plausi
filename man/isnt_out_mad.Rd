% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_funs.R
\name{isnt_out_mad}
\alias{isnt_out_mad}
\title{Z-score with MAD}
\usage{
isnt_out_mad(x, thres = 3, na.rm = TRUE)
}
\arguments{
\item{x}{variable of interest}

\item{thres}{z-score threshold (defaults to 3, which is a popular choice).}

\item{na.rm}{remove NAs, defaults to TRUE}
}
\value{
logical vector
}
\description{
Outlier detection based on MAD. Median Absolute Deviation is a robust normalization unit based on median as a population center. In order to use MAD “as a consistent estimator for the estimation of the standard deviation” one takes its value multiplied by a factor.
}
\examples{
x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)

isnt_out_mad(x)

}
