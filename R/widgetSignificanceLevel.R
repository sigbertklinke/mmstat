#' Widget for a significance level
#'
#' The widget allows to choose a significance level on a percent basis. 
#' This means for a 5\% significance level the result is 5 (and not 0.05).
#'
#' @param inputId widget name
#' @param level \code{sliderInput} parameter for the confidence level
#' @param lang language widget
#'
#' @seealso \code{\link{sliderInput}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the demo
#'   demo(testSignificanceLevel)
#'   demo(testSignificanceLevelLang)
#' }
widgetSignificanceLevel <- function(inputId,
													         level=list(),
													         lang=NULL) {
  env            <- widget(inputId, lang)
  args           <- list(inputId='level', label='Significance level (%)',
                              min=0.1, max=20, value=5, step=0.1)
  env[['ui']]$level  <- list(func='sliderInput',
                             args=mergeListsByName(args, level))
  return(env)
}
