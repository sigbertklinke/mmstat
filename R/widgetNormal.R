#' Widget for a normal distribution
#'
#' @param inputId character: widget name
#' @param mean \code{sliderInput} parameter for the mean of the normal distribution (default: \code{list()}, see Details)
#' @param sd \code{sliderInput} parameter for the standard deviation of the normal distribution (default: \code{list()}, see Details)
#' @param lang widget: language widget
#'
#' @details For \code{mean} and \code{sd} a named list with the parameters for 
#' \code{\link{sliderInput}} can be given. If just a numeric value is given then the according 
#' UI element is not drawn, but this value delivered back by \code{getValues}.
#'
#' @seealso \code{\link{sliderInput}}, \code{\link{dnorm}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   shinyDemo('testNormal')
#'   shinyDemo('testNormalConst')
#'   shinyDemo('testNormalLang')
#'   shinyDemo('testNormalInputId')
#'   shinyDemo('mmstatContinuousDistribution')
#' }
widgetNormal <- function(inputId,
												 mean=list(), sd=list(),
												 lang=NULL) {
  env              <- widget(inputId, lang)
  setUIelem (env, 'mean', 'sliderInput',  mean,
  				   list(inputId='mean', label=HTML('&mu;'), min=-5, max=5, value=0, step=0.1))
  setUIelem (env, 'sd', 'sliderInput', sd, 
  					 list(inputId='sd', label=HTML('&sigma;'), min=0, max=10, value=1, step=0.1))
  return(env)
}
