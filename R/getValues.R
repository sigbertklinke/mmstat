#' Input values for a widget
#'
#' @param env  widget
#' @param input current values of all of the widgets in your app. Is the first parameter of the \code{server} function.
#'
#' @return named list with the values
#' @export
#'
#' @examples
#' w <- widgetFontSize('widget')
#' getValues(w, NULL)
getValues <- function(env, input) {
	ret <- list()
	inp <- sapply(env[['ui']], function(l) { l$args$inputId } )
	ids <- paste(env[['widgetId']], inp, sep=".")
#	print(getInputs(env))
#	print(ids)
	for (uielem in env[['ui']]) {
		inp <- uielem$args$inputId
		ids <- paste(env[['widgetId']], inp, sep=".")
		if (uielem$func == "constant") {
			ret[[inp]] <- uielem$args$const
		} else {
			ret[[inp]] <- input[[ids]]
		}	
	}
	return(ret)
}