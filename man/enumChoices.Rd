% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enumChoices.R
\name{enumChoices}
\alias{enumChoices}
\title{Converts a \code{choices} parameter to a named list}
\usage{
enumChoices(..., select = NULL)
}
\arguments{
\item{...}{choices as text vector, single texts or list}

\item{select}{boolean vector with the same length as \code{...}}
}
\value{
a named list
}
\description{
The resulting named list contains integer values. The advantage is 
that the integer values are language independent. Only the list names will be
checked send through \code{getText}.
 
The parameter \code{select} allows to select only a part the current list.
}
\examples{
# Note: here the resulting list contains the integer values
enumChoices("Data set 1", "Data set 2", "Data set 3")
enumChoices(c("Data set 1", "Data set 2", "Data set 3"))
enumChoices(list("Data set 1", "Data set 2", "Data set 3"))
}
