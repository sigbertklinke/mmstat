#' Widget for a binomial distribution
#'
#' @param inputId character: widget name
#' @param size list: \code{sliderInput} parameter for the number of draws (default: \code{list()}, see Details)
#' @param prob list: \code{sliderInput} parameter for the probability of success per draw (default: \code{list()}, see Details)
#' @param lang widget: language widget
#'
#' @templateVar vars \code{size} and \code{prob}
#' @template detailvars
#'
#' @seealso \code{\link{sliderInput}}, \code{\link{dbinom}}
#'
#' @return  a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the demo
#'   demo(testBinomial)
#'   demo(testBinomialLang)
#'   demo(mmstatDiscreteDistribution)
#' }
widgetBinomial <- function(inputId,
											 		 size=list(), 
													 prob=list(),
													 lang=NULL) {
  env  <- widget(inputId, lang)
  setUIelem (env, 'size', 'sliderInput', size, 
            list(inputId='size', label='Number of draws', min=2,	max=30,	value=10, step=1))
  setUIelem (env, 'prob', 'sliderInput', prob,
    				list(inputId='prob', label='Probability of success per draw', min=0, max=1, value=0.2, step=0.05))
  return(env)
}
