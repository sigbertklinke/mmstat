#' Widget for a Poisson or exponential distribution
#'
#' @param inputId widget name
#' @param lambda \code{sliderInput} parameter for the intensity parameter
#' @param lang language widget
#' 
#' @seealso \code{\link{sliderInput}}, \code{\link{dpois}}, \code{\link{dexp}}
#'
#' @return  a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the demo
#'   demo(testIntensity)
#'   demo(testIntensityLang)
#'   demo(mmstatDiscreteDistribution)    # see Poisson distribution
#'   demo(mmstatContinuousDistribution)  # see Exponential distribution
#' }
widgetIntensity <- function(inputId,
														lambda=list(),
														lang=NULL) {
	env            <- widget(inputId, lang)
	args           <- list(inputId='lambda', label='Intensity',
												 min=0,	max=10, value=1, step=0.05)
	env[['ui']]$lambda <- list(func='sliderInput',
												     args=mergeListsByName(args, lambda))
	return(env)
}
