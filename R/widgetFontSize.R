#' Widget for font size choice
#'
#'
#' @param inputId widget name
#' @param fontsize \code{sliderInput} parameters for font size
#' @param lang language widget
#'
#' @seealso \code{\link{sliderInput}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   shinyDemo('testFontSize')
#'   shinyDemo('testFontSizeLang')
#' }
widgetFontSize <- function (inputId,
														fontsize=list(),
														lang=NULL) {
	env            <- widget(inputId, lang)
	args           <- list(inputId='fontsize', label='Font size',
												 min=0.75, max=1.5, value=1.25, step=0.05)
	env[['ui']]$fontsize  <- list(func='sliderInput',
									 	      		  args=mergeListsByName(args, fontsize))
	return(env)
}
