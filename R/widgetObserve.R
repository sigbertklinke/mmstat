mmstat.env <- new.env()

mmstat.env$pp <- list(actionButton       = list(update="updateActionButton", lang = "label"), 
											checkboxGroupInput = list(session = "selected", update="updateCheckboxGroupInput",
																								lang = "label", langnames = "choices"),
											checkboxInput      = list(session = "value", update="updateCheckboxInput",
																								lang = "label"),
											dateInput          = list(session = "value", update="updateDateInput",
																								lang = "label"),
											fileInput          = list(session = "value", update="updateFileInput",
																								lang = c("label", "buttonLabel", "placeholder")),
											numericInput       = list(session = "value", update="updateNumericInput",
																								lang = "label"),
											radioButtons       = list(session = "selected", update="updateRadioButtons",
																								lang = "label", langnames = "choices"),
											selectInput        = list(session = "selected", update="updateSelectInput",
																								input = "selected",
																								lang = "label", langnames = "choices"),
											sliderInput        = list(session = "value", update="updateSliderInput",
																								lang = "label"),
											textInput          = list(session = "value", update="updateTextInput",
																								lang =  c("label", "value", "placeholder"))
)

#' getShinyInfo
#'
#' Information for input functions in the package \code{shiny} are stored in the library \code{mmstat}.
#' To access the information this function is used.
#'
#' @param funcname character: name of shiny function
#'
#' @return information about a specific shiny input function
#' @export
#'
#' @examples
#' getShinyInfo('sliderInput')
getShinyInfo <- function(funcname) {
	return(mmstat.env$pp[[funcname]])
}

#' Create a reactive observer
#'
#' Creates an observer from the given widget. The observer handles internal relationships between the elements of a widget.
#'
#' @param env widget object
#' @param input input object: \code{input} parameter from the server function of the app
#' @param session session object: \code{session} parameter from the server function of the app
#'
#' @return a observer for the Shiny app
#' @export
widgetObserve <- function (env, input, session) {
	if (is.list(env[['observe']])) return(sapply(env[['observe']],  function(x, input, session) { eval(x) }, input=input, session=session))
	return(eval(env[['observe']]))
}