#' Widget for a one parameter test
#'
#' If \code{alternative} or \code{level} ist set to NULL then the corresponding widget element is not displayed.
#'
#' @param inputId widget name
#' @param alternative \code{selectInput} parameter for the alternative: two sided, less or greater (or NULL)
#' @param parameter \code{sliderInput} parameter for the hypothetical parameter
#' @param level \code{sliderInput} parameter for the significance level (or NULL)
#' @param lang language widget (or NULL)
#'
#' @seealso \code{\link{selectInput}}, \code{\link{sliderInput}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   shinyDemo('testTest')
#'   shinyDemo('testTestLang')
#' }
widgetTest <- function(inputId,
											 alternative  = list(),
											 parameter    = list(),
											 level        = list(),
											 lang=NULL) {
  env <- widget(inputId, lang)
	# alternative
	if (!is.null(alternative)) {
  	args <- list(inputId='alternative',	label='Choose alternative',
						  	 choices = list("Two sided" = "two.sided", "Less" = "less", "Greater" = "greater"))
  	env[['ui']]$alternative <- list(func='radioButtons',
  	                                args=mergeListsByName(args, alternative))
	}
	# parameter
	args <- list(inputId='parameter', label='Hypothetical parameter',
							 min=-5, max=5, value=0, step=0.1)
  env[['ui']]$parameter <- list(func='sliderInput',
                                args=mergeListsByName(args, parameter))
	# significance level
  if (!is.null(alternative)) {
  	args <- list(inputId='level', label='Significance level (%)',
	   						 min=0.1, max=20, value=5,	step=0.1)
	  env[['ui']]$level     <- list(func='sliderInput',
	                               args=mergeListsByName(args, level))
  }
	#
	return(env)
}
