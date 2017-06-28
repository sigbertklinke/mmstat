#' Input names for the widget elements
#'
#' @param env widget
#'
#' @return The names of the inputs which are used inside the widget
#' @export
#'
#' @examples
#' w <- widgetFontSize('widget')
#' getInputs(w)
getInputs <- function(env) {
	inp <- sapply(env[['ui']], function(l) { l$args$inputId} )
	paste(env[['widgetId']], inp, sep=".")
}