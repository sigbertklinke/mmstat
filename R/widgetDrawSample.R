#' Widget for drawing a samples(s) from a population
#'
#' If the widget elements \code{size}, \code{reset} or \code{speed} are set to NULL then 
#' they will not display or used.
#'
#' @param inputId widget name
#' @param size list: \code{sliderInput} parameter for the sample size (or NULL)
#' @param sample \code{actionButton} parameter for drawing one sample
#' @param reset \code{actionButton} parameter to reset the sample drawing (or NULL)
#' @param speed \code{sliderInput} parameter for repeatedly drawing samples (or NULL)
#' @param lang language widget
#'
#' @seealso \code{\link{sliderInput}}, \code{\link{actionButton}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   shinyDemo('testDrawSample')
#'   shinyDemo('testDrawSampleLang')
#' }
widgetDrawSample <- function(inputId,
												  	 size=list(),
														 sample=list(),
														 reset=list(),
														 speed=list(),
														 lang=NULL) {
	env               <- new.env()
	env[['widgetId']] <- inputId
	env[['lang']]     <- lang
	# size
	if (!is.null(size)) {
		env[['ui']]$size <- list(func='sliderInput',
			args=mergeListsByName(size,
			 										  list(inputId='size',
					 									label="Sample size",
					 									min=30,
					 									max=100,
					 									value=30,
					 									step=1))	
					 )
	}
  # sample
	env[['ui']]$sample <- list(func='actionButton',
		args=mergeListsByName(sample,
													list(inputId='sample', 
															 label="Draw sample")))
	env[['change']]$sample <- 0
  # reset
  if (!is.null(reset)) {
  	env[['ui']]$reset <- list(func='actionButton',
  														args=mergeListsByName(reset,
   																		 list(inputId='reset', 
  																		 		  label="Reset")))
  	env[['change']]$reset <- 0
  }
  # speed
  if (!is.null(speed)) {
  	env[['ui']]$speed <- list(func='sliderInput',
  														args=mergeListsByName(speed,
  																		list(inputId='speed',
  																				 label=NULL,
  																				 min=0,
  																				 max=5,
  																				 value=0,
  																				 step=0.5)))
  }
	#
	return(env)
}
