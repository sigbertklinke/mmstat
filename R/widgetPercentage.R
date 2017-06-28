#' Widget for a percentage value
#'
#' @param inputId widget name
#' @param p \code{sliderInput} parameter for the percentage
#' @param lang language widget (or NULL)
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   demo(testPercentage)
#'   demo(testPercentageLang)
#' }
widgetPercentage <- function(inputId,
													   p=list(),
														 lang=NULL) {
	env            <- widget(inputId, lang)
	args           <- list(inputId='p', label="Percentage", 
												 min=0, max=1, value=0.5, step=0.01)
	env[['ui']]$p  <- list(func='sliderInput',
    	                   args=mergeListsByName(args, p))
  return(env)
}