% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_funs.R
\name{predict_votes}
\alias{predict_votes}
\title{Run predictions for multiple columns (specifically votes) in a dataset}
\usage{
predict_votes(
  votes,
  train,
  test = NULL,
  method = "bagEarth",
  trainControl = NULL,
  exclude_votes = FALSE,
  ...
)
}
\arguments{
\item{votes}{names of the dependent variable-columns}

\item{train}{data used to train the model containing the variables to be predicted and the predictor-columns}

\item{test}{optional dataset structured identically as the trainingdataset on which the prediction}

\item{method}{method available in the caret-package which should be used for the prediction}

\item{trainControl}{parameters to tune the model}

\item{exclude_votes}{if TRUE the variables to be predicted will be excluded from each others models}

\item{...}{optional parameters that can be passed to the caret::train function}
}
\value{
data.frame
}
\description{
Run predictions for multiple columns (specifically votes) in a dataset
}
\examples{

predict_votes(c("Eidg1","Kant1"), votedata, exclude_votes=TRUE)
}
