% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normReads.R
\name{normalizeReads}
\alias{normalizeReads}
\title{normalizeReads}
\usage{
normalizeReads(readsMatrix)
}
\arguments{
\item{readsMatrix}{a reads matrix with samples in columns and targets in lines (the first column are the targets)}
}
\value{
a normalized reads matrix
}
\description{
normalizeReads
}
\examples{
normReads <- normalizeReads(readsMatrixExample)
}
