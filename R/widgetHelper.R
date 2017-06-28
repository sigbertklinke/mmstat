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
mmstat.env$col   <- list(daquamarine="#1B9E77",  dorange="#D95F02",   dblue="#7570B3",   dpink="#E7298A", 	
											   dgreen="#66A61E",     	dyellow="#E6AB02", 	dgold="#A6761D", 	dgray="#666666",
											   laquamarine="#66C2A5",  lorange="#FC8D62",  lblue="#8DA0CB",  lpink="#E78AC3", 	
											   lgreen="#A6D854",     	lyellow="#FFD92F", 	lgold="#E5C494", 	lgray="#B3B3B3")
mmstat.env$alpha <- c(0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20)
mmstat.env$col.sample            <- mmstat.env$col[1]
mmstat.env$col.sample.robust     <- mmstat.env$col[3]
mmstat.env$col.population        <- mmstat.env$col[2]
mmstat.env$col.population.robust <- mmstat.env$col[4]

getShinyInfo <- function(funcname) {
	return(mmstat.env$pp[funcname])
}

#' mmstatPar
#'
#' Sets and queries parameter values by mmstat.
#'
#' @param ... list: list of named parameter to set a parameter or characters to question the current
#' value of parameter in mmstat
#'
#' @details The following parameters are available for setting and getting
#' \itemize{
#' \item{\code{col}}{ a vector of eight light and dark colors which are generated with the R Color brewer}
#' \item{\code{col.sample}}{ color used in mmstat to draw sample data/parameters}
#' \item{\code{col.sample.robust}}{ color used in mmstat to draw robust sample parameters}
#' \item{\code{col.population}}{ color used in mmstat to draw population data/parameters}
#' \item{\code{col.population.robust}}{ color used in mmstat to draw robust population parameters}
#' \item{\code{alpha}}{ the default set for possible significance levels}
#' } 
#'
#' @return a named list with the old parameter values
#' @export
#'
#' @examples
#' mmstatPar('col.sample')
#' mmstatPar(col.sample = "blue", col.population = "red")
mmstatPar <- function(...) {
	readonly <- c("pp")
	args <- list(...)
	ret  <- list()
	if(!length(args)) return(as.list(mmstat.env))
	argname <- names(args)
	if (!length(argname)) argname <- rep('', length(args))
	 for (i in 1:length(args)) {
	  if (argname[i]=='') {
	  	fail <- T 
	  	if (is.character(args[[i]])) {
	  		fail <- F
	  		ret[[args[[i]]]] <- mmstat.env[[args[[i]]]]
	  	} 
	  	if (is.list(args[[i]])) {
	  		fail <- F
	  		ret <- c(ret, do.call('mmstatPar', args[[i]]))
	  	} 
	  	if (fail) ret[i] <- list(NULL)
	  } else {
	  	ret[[argname[[i]]]] <- mmstat.env[[argname[[i]]]]
	  	if (!is.na(match(argname[[i]], readonly))) stop(sprintf('Parameter "%s" is read only', argname[[i]])) 
	  	mmstat.env[[argname[[i]]]] <- args[[argname[[i]]]]
	  }
	}
	ret
}

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
#'   demo(testDrawSample)
#'   demo(testDrawSampleLang)
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
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   demo(testDrawSample)
#'   demo(testDrawSampleLang)
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

#' Converts a \code{choices} parameter to a named list
#'
#' The resulting named list contains, if possible, integer values. The advantage is 
#' that the integer values are language independent. Only the list names will be
#' checked send through \code{getText}.
#'  
#' The parameter \code{select} allows to select only a part the current list.
#'
#' @param txt choices
#' @param select boolean vector with the same length as \code{txt}
#'
#' @return a named list
#' @export
#'
#' @examples
#' # Note: here the resulting list contains the integer values
#' enumChoices(c("Data set 1", "Data set 2", "Data set 3"))
#' enumChoices(list("Data set 1", "Data set 2", "Data set 3"))
#' # Note: here the resulting list contains the original values
#' enumChoices(c(a="Data set 1", b="Data set 2", c="Data set 3"))
enumChoices <- function(txt, select=NULL) {
  if (is.null(names(txt))) {
    ret <- as.list(seq(length(txt)))
    names(ret) <- txt
  } else {
    ret <- as.list(txt)
    names(ret) <- names(txt)
  }
	if (!is.null(select)) ret[!select] <- NULL
	ret
}

#' Create a reactive observer
#'
#' Creates an observer from the given widget. The observer handles internal relationships between the elements of a widget.
#'
#' @param env widget 
#' @param input \code{input} parameter from the server function of the app
#' @param session \code{session} parameter from the server function of the app
#'
#' @return a observer for the Shiny app
#' @export
widgetObserve <- function (env, input, session) {
	if (is.list(env[['observe']])) return(sapply(env[['observe']],  function(x, input, session) { eval(x) }, input=input, session=session))
  return(eval(env[['observe']]))
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