#' Input values for a widget
#'
#' Returns a named list of input values for widget. If \code{simplify} is set to \code{TRUE} then a list with 
#' one element just returns this element instead of a list.
#'
#' @param env  widget
#' @param input list: named list of input values in your app. It is the first parameter of the \code{server} function.
#' @param simplify logical: should return be simplified (default: \code{FALSE})
#'
#' @return named list with the values
#' @export
#'
#' @examples
#' w <- widgetFontSize('widget')
#' getValues(w, NULL)
getValues <- function(env, input, simplify=FALSE) {
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
	if (simplify && (length(ret)==1)) {
	  nret <- names(ret)
	  ret  <- ret[[1]]
	  if (!is.null(ret)) names(ret) <- nret
	}
	return(ret)
}