% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widgetSignificanceLevel.R
\name{widgetSignificanceLevel}
\alias{widgetSignificanceLevel}
\title{Widget for a significance level}
\usage{
widgetSignificanceLevel(inputId, level = list(), lang = NULL)
}
\arguments{
\item{inputId}{widget name}

\item{level}{\code{sliderInput} parameter for the confidence level}

\item{lang}{language widget}
}
\value{
a widget object (environment)
}
\description{
The widget allows to choose a significance level on a percent basis. 
This means for a 5\% significance level the result is 5 (and not 0.05).
}
\examples{
\dontrun{
  shinyDemo('testSignificanceLevel')
  shinyDemo('testSignificanceLevelLang')
}
}
\seealso{
\code{\link{sliderInput}}
}
