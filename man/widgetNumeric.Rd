% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widgetShiny.R
\name{widgetNumeric}
\alias{widgetNumeric}
\title{Encapsulates a Shiny \code{numericInput}}
\usage{
widgetNumeric(param, lang)
}
\arguments{
\item{param}{list of parameters for \code{numericInput}}

\item{lang}{language widget (or NULL)}
}
\value{
a widget object (environment)
}
\description{
Encapsulates a Shiny \code{numericInput}t. If you do not need a language change on-the-fly then you should use \code{numericInput}.
}
\examples{
widgetNumeric(list(inputId='obs', label='Observations:', value=10, min=1, max=100), lang=NULL)
}
\seealso{
\code{\link{numericInput}}
}
