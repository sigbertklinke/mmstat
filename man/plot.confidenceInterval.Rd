% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidenceInterval.R
\name{plot.confidenceInterval}
\alias{plot.confidenceInterval}
\title{plot.confidenceInterval}
\usage{
\method{plot}{confidenceInterval}(x, population = list(pch = 19, cex = 0.5,
  col = getOption("mmstat.col.population")[1]), sample = list(pch = 19, cex =
  0.25, col = getOption("mmstat.col.sample")[1]), ci.lines = list(lwd = 1, col
  = c("black", "red")), ci.points = list(pch = 19, cex = 0.5, col = c("black",
  "red")), param.abline = list(col = "red"), text.population = "Population",
  ...)
}
\arguments{
\item{x}{confidence interval object}

\item{population}{list: parameters to draw the population points}

\item{sample}{list: parameters to draw the sample points}

\item{ci.lines}{list: parameters to draw the confidence interval}

\item{ci.points}{list: parameters to draw the estimated parameter}

\item{param.abline}{list: parameters to draw the line of the true parameter}

\item{text.population}{character: text to write for the population}

\item{...}{further parameters given \code{\link[graphics]{plot}}}
}
\value{
plot
}
\description{
plot.confidenceInterval
}
\examples{
x  <- runif(100) # generate some population
ci <- confidenceInterval(x, CI.mean)
plot(ci)
}
