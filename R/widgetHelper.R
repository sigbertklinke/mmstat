#' mergeListsByName
#'
#' Merges two or more lists by element names. If two lists contain an element with the same name then the 
#' element from the last list will become the element of the resulting list.
#'
#' @param ... list: a list of named lists
#'
#' @return a merged list
#' @export
#'
#' @examples
#' default.parameters <- list(a=1, b=1)
#' use.parameters <- list(a=3, c=1)
#' mergeListsByName(default.parameters, use.parameters)
mergeListsByName <- function (...) {
	ret  <- list()
	args <- list(...)
	for (arg in args) { # works if list element is NULL
		for (name in names(arg)) ret[name] <- arg[name]
	}
	ret
}

#' Checks if any parameter is NULL or a list of length zero
#'
#' When Shiny starts all reactive expression are run through with empty input parameters, 
#' e.g. for initialising variables for the expression. Mostly the output can only generated if 
#' all input parameters are set/given. Thus \code{anyUndefined} delivers \code{FALSE} is any 
#' cecked \code{input} parameter is not set.
#'
#' @param ... list of parameters to check
#'
#' @return boolean
#' @export
#'
#' @examples
#' anyUndefined(list(), NULL)
anyUndefined <- function(...) { 
	any(sapply(list(...), is.null) |
		  sapply(list(...), function(e) {length(e)==0}))
}

#' Is a widget value changed
#'
#' If you use a button then the corresponding element in \code{input} delivers the number of times
#' the button was pressed. \code{widgetValueChanged} returns a list with information if the 
#' internally  stored value is different from the corresponding element in \code{input} and 
#' stores the new value.
#'
#' @param env a widget object
#' @param input current values of all of the widgets in your app. Is the first parameter of the \code{server} function.
#'
#' @seealso \code{\link{widgetActionButton}}
#'
#' @return named list with the logical values
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   shinyDemo('testDrawSample')
#'   shinyDemo('testDrawSampleLang')
#' }
widgetValueChanged <- function(env, input) {
	if (is.null(env[['change']])) return(NULL)
	ret <- list()
	nch <- names(env[['change']])
	inp <- paste(env[['widgetId']], nch, sep='.')
	for (i in seq(nch)) {
		if (!is.null(input[[inp[i]]])) {
  		ret[[nch[i]]] <- (env[['change']][[nch[i]]]!=input[[inp[i]]])
      if (ret[[nch[i]]]) env[['change']][[nch[i]]] <- input[[inp[i]]]
		} else
			ret[[nch[i]]] <- F
	}
	ret
}

#' Invalidate the current reactive context
#'
#' Schedules the current reactive context to be invalidated in the given number of milliseconds. 
#' If \code{millis=NULL} then the waiting time is set to 500/\code{value}.
#'
#' @param value A parameter which determines how fast the current reactive context will be invalidated
#' @param millis Approximate milliseconds to wait before invalidating the current reactive context.
#' @param session A session object. This is needed to cancel any scheduled invalidations after 
#'                a user has ended the session. If NULL, then this invalidation will not be 
#'                tied to any session, and so it will still occur.
#'                
#' @seealso \code{\link{invalidateLater}}, \code{\link{widgetDrawSample}}                  
#'                
#' @details If this is placed within an observer or reactive expression, that object will be 
#' invalidated (and re-execute) after the interval has passed. The re-execution will reset 
#' the invalidation flag, so in a typical use case, the object will keep re-executing and 
#' waiting for the specified interval. It's possible to stop this cycle by adding conditional 
#' logic that prevents the invalidateLater from being run.
#'
#' @export
#' @import shiny
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   shinyDemo('testDrawSample')
#'   shinyDemo('testDrawSampleLang')
#' }
widgetInvalidate <- function(value, millis=NULL, session=getDefaultReactiveDomain()) {
	if (!is.null(value) && (value>0)) {
		if (is.null(millis)) value <- 500/value
		invalidateLater(value, session)
	}
}

#' Prints the contents of an R object
#'
#' @param v R object
#'
#' @return Shows the contents of \code{v} using \code{str}
#' @export
#'
#' @examples
#' x <- 1:10
#' printVar(x)
printVar <- function(v) {
	cat(deparse(substitute(v)))
	cat("\n")
	str(v)
}

#' Stops execution
#'
#' Stops the execution of a Shiny app, if at least \code{cond} contains one \code{TRUE}. The function uses \code{validate} and \code{need} from the library \code{shiny}.
#'
#' @param cond condition, execution is stopped if any entry is \code{TRUE}
#' @param fmt a character vector of format strings for \code{sprintf} 
#' @param ... further values to be passed into \code{fmt}. Only logical, integer, real and character vectors are supported.
#'
#' @export
#' @import utils
#'
#' @examples
#' \dontrun{
#'   # Produces an error
#'   x <- -1
#'   stopif(x<=0, "x is smaller than %i", 0)
#' }
stopif <- function(cond, fmt, ...) {
	cond <- any(cond)
  if (cond) {
    args <- list(...)
    args$fmt <- fmt
    validate(need(!cond, do.call('sprintf', args)))
  }
}

setUIelem <- function (env, elem, func, arg, defaults) {
  if (!is.null(arg)) {
    if (is.list(arg))	{
      env[['ui']][[elem]] <- list(func=func, args=mergeListsByName(defaults, arg))
    } else { 
      defaults$const <- arg
      env[['ui']][[elem]] <- list(func='constant', args=defaults)
    }
  }
}
