% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combinatorics.R
\name{combination}
\alias{combination}
\title{combination}
\usage{
combination(n, k, rep = F)
}
\arguments{
\item{n}{numeric: number of elements to select from}

\item{k}{numeric: number of elements selected}

\item{rep}{logical: with or without repetition (default: FALSE)}
}
\value{
numeric
}
\description{
Combinations are arrangements of selections of objects, where the order of the selected objects does not matter.
\code{combination} computes the number of \code{k}-element combinations of \code{n} objects either with 
repetition or not.
}
\details{
\itemize{
\item{without repetition}{\eqn{C(n,k)=\choose{n}{k}}}
\item{with repetition}{\eqn{C(n,k)=\choose{n+k-1}{k}}}
}
}
\examples{
combination(10, 3)
combination(10, 3, TRUE)
}
