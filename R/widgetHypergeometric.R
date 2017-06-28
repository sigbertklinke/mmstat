#' Widget for a hypergeometric distribution
#'
#' This widget needs an \code{\link{widgetObserve}} to update \code{M} and \code{n} if \code{N} changes.
#'
#' @param inputId widget name
#' @param N \code{sliderInput} parameter for the population size
#' @param M \code{sliderInput} parameter for the number of success states in the population
#' @param n \code{sliderInput} parameter for the number of draws
#' @param lang language widget
#'
#' @details Note that the widget does not follow the parameter definition of \code{dhyper}. 
#' The parameter \code{N} from \code{widgetHypergeometric} is equal to \code{m+n} from \code{dhyper}.
#'
#' @seealso \code{\link{sliderInput}}, \code{\link{dhyper}}
#'
#' @return  a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the demo
#'   demo(testHypergeometric)
#'   demo(testHypergeometricLang)
#'   demo(mmstatDiscreteDistribution)
#' }
widgetHypergeometric <- function(inputId,
								 							 	 N=list(), M=list(), n=list(),
																 lang=NULL) {
  env  <- widget(inputId, lang)
	# N
	args <- list(inputId='N', label='Population size',
							 min=1, max=50, value=20, step=1)
	env[['ui']]$N <- list(func='sliderInput',
	                      args=mergeListsByName(args, N))
	N <- env[['ui']]$N$args$value
	# M
	args <- list(inputId='M', label='Number of success states in the population',
							 min=1, max=N, value=1, step=1)
	env[['ui']]$M <- list(func='sliderInput',
	                      args=mergeListsByName(args, M))
	# n
	args <- list(inputId='n', label='Number of draws',
							 min=1, max=N, value=1, step=1)
	env[['ui']]$n <- list(func='sliderInput',
	                      args=mergeListsByName(args, n))
	### observer
	env[['observe']] <- expression(observe({
	  inp <- getInputs(env)
		N <- input[[inp[1]]]
		M <- isolate(input[[inp[2]]])
		n <- isolate(input[[inp[3]]])
		if (!is.null(N)) {
      updateSliderInput(session, inp[2], max=N, value=min(M,N))
      updateSliderInput(session, inp[3], max=N, value=min(n,N))
		}
	}))
	return(env)
}
