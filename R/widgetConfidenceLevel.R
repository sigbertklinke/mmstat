#' Widget for a confidence level
#'
#' The widget allows to choose a confidence level on a percent basis. 
#' This means for a 95\% confidence level the result is 95 (and not 0.95).
#'
#' @param inputId character: widget name
#' @param level list: \code{sliderInput} parameter for the confidence level  (default: \code{list()}, see Details)
#' @param lang widget: language widget
#'
#' @details For \code{level} a named list with the parameters for 
#' \code{\link{sliderInput}} can be given. If just a numeric value is given then the according 
#' UI element is not drawn, but this value delivered back by \code{getValues}.
#'
#' @seealso \code{\link{sliderInput}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   demo(testConfidenceLevel)
#'   demo(testConfidenceLevelLang)
#' }
widgetConfidenceLevel <- function(inputId,
													        level=list(),
																  lang=NULL) {
	env <- widget(inputId, lang)
	setUIelem (env, 'level', 'sliderInput', level, 
						 list(inputId='level', label='Confidence level (%)',
	           min=80, max=99.9, value=95, step=0.1))
	return(env)
}
