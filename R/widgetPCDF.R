#' Widget for selecting between functions
#'
#' You can select between a probability mass function/probability density function.
#' A button allows to refit the axes to current display function.
#'
#' @param inputId widget name
#' @param pcdf \code{radioButtons} parameter for the function selection
#' @param refit \code{actionButton} parameter to reset the axes
#' @param lang language widget
#'
#' @seealso \code{\link{radioButtons}}, \code{\link{actionButton}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   shinyDemo('testPCDF')
#'   shinyDemo('testPCDFLang')
#' }
widgetPCDF <- function(inputId,
											 pcdf=list(),
											 refit=list(),
											 lang=NULL) {
	env               <- new.env()
	env[['widgetId']] <- inputId
	env[['lang']]     <- lang
	# pcdf
	args <- list(inputId='pcfunc', label="Choose",
							 choices=c("Probability mass function", 
												 "Cumulative distribution function")
							)
	env[['ui']]$pcdf <- list(func='radioButtons',
													 args=mergeListsByName(pcdf, args))	
  # refit
	args <- list(inputId='refit', label="Refit axes")
	env[['ui']]$refit<- list(func='actionButton',
													 args=mergeListsByName(refit,args))
	env[['change']]$refit <- 0
	#
	return(env)
}
