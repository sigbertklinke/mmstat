% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidenceInterval.R
\name{confidenceInterval}
\alias{confidenceInterval}
\alias{confidenceInterval.default}
\title{confidenceInterval}
\usage{
confidenceInterval(x, ...)

\method{confidenceInterval}{default}(x, CI, param = NULL, sample = 25,
  size = length(x)/2, discrete = FALSE, replace = TRUE, ...)
}
\arguments{
\item{x}{numeric: vector of population values}

\item{...}{further parameters}

\item{CI}{function: computes a confidence interval, see details}

\item{param}{numeric: true parameter from the population if known (default: \code{NULL})}

\item{sample}{numeric: number of samples to compute (default: \code{25})}

\item{size}{numeric: sample sizes to use for the confidence interval computation (default: \code{length(x)/2})}

\item{discrete}{logical: if population is discrete (default: \code{FALSE})}

\item{replace}{logical: draw sample with replacement or not (default: \code{TRUE})}
}
\value{
\code{NULL}
}
\description{
Creates confidence intervals for a single parameter
}
\details{
...
}
\examples{
ci <- confidenceInterval(runif(100), CI=CI.mean)
}
