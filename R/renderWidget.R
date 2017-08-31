#' Renders a widget
#'
#' Renders the complete widget or a element of it. 
#'  
#' If the parameter \code{lang} is not NULL then it should contain the number of the 
#' selected language, usually it will be the \code{input[[getInputs(language widget)]]}.
#' If the parameter \code{session} is not NULL and the \code{value} of a slider element
#' is set to "session" then the starting value is taken from the URL. 
#' 
#' E.g. let \code{l <- widgetLanguage('lang')} then the input element will be called 
#' \code{lang.language}. Calling the Shiny app with \code{http://...?lang.language=2} will
#' result that app start with second language as default rather then with the first.
#'  
#' @param env widget
#' @param session session parameter of the server function
#' @param elem element of widget to render, if NULL the whole widget will be rendered
#'
#' @note A starting value of a widget element by URL can be only set at the beginning of 
#' session. Reloading the page will NOT start a new shiny session. 
#'
#' @return HTML code which can be used instead of \code{renderUI}
#' @export
#'
#' @examples
#' w <- widgetFontSize('widget')
#' renderWidget(w)
#' 
#' \dontrun{
#'   # Open in browser and add "?widget.size=11" to the URL to set the starting
#'   # value to 11
#'   shinyDemo('testBinomialSession')
#' }
renderWidget <- function(env, session=NULL, elem=NULL) {
	# find ui elems
  if (!is.null(elem)) {
    pos <- pmatch(elem, names(env[['ui']]))
  } else {
    pos <- seq(env[['ui']])
  }
	# prepare for session
	hasSession <- !is.null(session)
	query <- parseQueryString(session$clientData$url_search)
	# render elements
  ret <- list()
  for (i in 1:length(pos)) {
    posi <- pos[i]
    if (!is.na(posi)) {
      func <- env[['ui']][[posi]]$func
      id   <- env[['ui']][[posi]]$args$inputId
      args <- env[['ui']][[posi]]$args
      args$inputId <- paste(env[['widgetId']], id, sep=".")
      # just at first time parameter can be set by URL
      if (hasSession) { 
      	ppp <- getShinyInfo(func)$session
      	if (!is.null(ppp)) {
      	  posq  <- which(names(query)==args$inputId) 
      	  if (length(posq)==1) {
      	  	env[['ui']][[i]]$args[[ppp]] <- args[[ppp]] <- query[[posq]]
      	  }
      	}
      }
      if (func!='constant') {
      	ret[[i]] <- do.call(func, args)
      }
      tooltip <- env[['ui']][[posi]]$tooltip
      if(!is.null(tooltip)) {
      	if (is.character(tooltip)) {
      		targs <- list(title=tooltip, placement="right", trigger="hover")
      	} else {
      		targs         <- tooltip
      		targs$id      <- args$inputId
      	}
      	targs$el <- ret[[i]]
      	ret[[i]] <- do.call('tipify', targs)	
      }
    }
  }
  return(ret)
}
