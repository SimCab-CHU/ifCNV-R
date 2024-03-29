% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateScore.R
\name{calculateScore}
\alias{calculateScore}
\title{calculate Score}
\usage{
calculateScore(
  readsMatrix,
  abSamples,
  abTargets,
  roi = "Gene",
  sep = "-",
  thrScore = 7
)
}
\arguments{
\item{readsMatrix}{a reads matrix with samples in columns and targets in lines (the first column are the targets)}

\item{abSamples}{the abSamples list result of the abSamples function}

\item{abTargets}{a list of dataframes of the targets tagged as outliers result of the abTargets() function}

\item{roi}{the region of interest (Gene or Gene-Exon)}

\item{sep}{a character the separator between roi in the bed file}

\item{thrScore}{(default 0) a threshold on the localization score}
}
\value{
the score associated with the abSamples in the desired roi
}
\description{
calculate Score
}
\examples{
abS <- abSamples(readsMatrixExample)
abT <- abTargets(readsMatrixExample,abSamples(readsMatrixExample))
calculateScore(readsMatrixExample, abS, abT, sep="-")
}
