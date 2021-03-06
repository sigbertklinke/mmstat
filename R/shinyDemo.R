#' shinyDemo
#'
#' Runs a shiny app similar to \code{demo}.
#'
#' @param example character: name of the example
#' @param launch.browser logical: 	
#' If true, the system's default web browser will be launched automatically after the app is started (default: \code{FALSE})
#'
#' @references Based on \href{http://deanattali.com/2015/04/21/r-package-shiny-app/}{Supplementing your R package with a Shiny app} by Dean Attali. 
#' @export
#'
#' @examples
#' \dontrun{
#'   # to view all possible apps
#'   shinyDemo()
#'   # to run one app 
#'   shinyDemo('mmstatContinuousDistribution.R')
#' }
shinyDemo <- function(example, launch.browser=getOption("shiny.launch.browser", interactive())) {
	# locate all the shiny app examples that exist
	validExamples <- list.files(system.file("shiny-demo", package = "mmstat"), pattern="*.R")
	
	validExamplesMsg <-
		paste0(
			"Valid examples are: '",
			paste(validExamples, collapse = "', '"),
			"'")
	
	# if an invalid example is given, throw an error
	validExample <- NULL
	if (!missing(example) && nzchar(example)) {
		if (example %in% validExamples) {
			validExample <- example
		} else {
			example <- paste0(example, '.R')
			if (example %in% validExamples) validExample <- example
		}
	}
  if (is.null(validExample)) {
	  stop('Please run `shinyDemo()` with a valid example app as an argument.\n',
		     validExamplesMsg,
		     call. = FALSE)	
  }
	# find and launch the app
	appDir <- system.file("shiny-demo", validExample, package = "mmstat")
	shiny::runApp(appDir, display.mode = "normal", launch.browser=launch.browser)
}