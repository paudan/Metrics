% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.R
\name{sle}
\alias{sle}
\title{Squared Log Error}
\usage{
sle(actual, predicted, na.rm = F)
}
\arguments{
\item{actual}{The ground truth non-negative vector}

\item{predicted}{The predicted non-negative vector, where each element in the vector
is a prediction for the corresponding element in \code{actual}.}

\item{na.rm}{Whether to remove NA/NaN values; if those are not removed, output will usually be \code{NA}}
}
\description{
\code{sle} computes the elementwise squares of the differences in the logs of two numeric vectors.
}
\details{
\code{sle} adds one to both \code{actual} and \code{predicted} before taking
the natural logarithm of each to avoid taking the natural log of zero. As a result,
the function can be used if \code{actual} or \code{predicted} have zero-valued
elements. But this function is not appropriate if either are negative valued.
}
\examples{
actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
sle(actual, predicted)
}
\seealso{
\code{\link{msle}} \code{\link{rmsle}}
}
