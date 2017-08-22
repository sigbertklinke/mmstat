#' Text translation: getText
#'
#' Each element of the text vector \code{msg} is translated in the current target language. 
#' If the translation of a text element is not possible then the original text is delivered.
#'  
#' @param msg character: text vector to translate
#' @param env widget: language widget
#'
#' @details For the translations must be PO files provided. If a Shiny app 
#' is run locally (not under a server) \code{getText} writes in the current directory a file
#' \code{getText.log} which contains elements of \code{msg} in a form such that it can be 
#' parsed with \code{xgettext getText.log}. You may use \code{Poedit} to create and update
#' your PO files.
#' 
#' @seealso \code{\link{selectInput}}
#' 
#' @note Since the analysis of the PO files is done by an R function in the package, 
#' not all features which PO file allows are available.
#' 
#' @return For all elements of the text vector a translation will be delivered.
#' If no translation is available then the original text will be given back.
#' @export
#'
#' @examples
#' getText('GERMAN', NULL)
getText <- function(msg, env=NULL) {
	if (is.null(env)) return(msg)
	if (env[['log']]) cat(sprintf('gettext("%s");\n', msg), file='getText.log', append=T)
  sel <- env[['selected']]
  if (length(sel)==0) return(msg)
	ret <- msg
	pos <- match(msg, env[['data']][[sel]]$id)
	ind <- (1:length(pos))[!is.na(pos)]
	ret[ind] <- env[['data']][[sel]]$str[pos[ind]]
	return(ret)
}

#' Text translation: getText
#'
#' Each text element of the R object \code{obj} is translated in the current target language. 
#' If the translation of a text element is not possible then the original text is delivered. 
#'
#' @param obj R object: 
#' @param env widget: language widget
#' 
#' @details For the translations must be PO files provided. Since \code{\link{getText}} is used 
#' internally the same holds as in \code{\link{getText}} given. Currently are only tables as 
#' objects supported, all other objects will deliver a warning.
#' 
#' @return a R object witj translated texts
#' @export
#'
#' @examples 
#' lang <- widgetLanguage ('lang')
#' tab  <- table(round(rnorm(100)), round(rnorm(100)))
#' dimnames(tab) <- c("GERMAN", "ENGLISH")
#' poText(tab, lang)
poText <- function(obj, env=NULL) {
	if (is.null(env) || is.null(obj)) return(obj)
	ret               <- obj
	translated        <- rep(FALSE, length(class(ret)))
	names(translated) <- class(ret)
	if ('table' %in% class(ret)) {
		attr <- attributes(ret)
		names(attr$dimnames) <- getText(names(attr$dimnames), env)
		if (length(attr$dimnames)) {
			for (i in 1:length(attr$dimnames)) {
				attr$dimnames[[i]] <- getText(attr$dimnames[[i]], env)
			}
		}
		if ('character' %in% class(obj[1])) {
			ret <- getText(ret, env)
		}
		attributes(ret) <- attr
		translated['table'] <- TRUE
	}
	if (any(!translated)) warning(sprintf('not translated: %s', paste0(names(translated[!translated]), collapse = ",")))
	ret
}

#' Widget for language choice
#' 
#' For the translations must be PO files provided. Beside creating the widget also the PO file 
#' are read and stored internally.
#'
#' @param inputId widget name
#' @param lang \code{selectInput} parameter for selecting a language
#' @param path directory where the PO files are found. The default location is the directory where
#' the R the \code{shinyWidgets} package stores.
#' 
#' @seealso \code{\link{selectInput}}
#' 
#' @details To find the location of the PO files use 
#' \code{system.file(package='shinyWidgets')}
#'
#' @return  a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'  shinyDemo('testLanguage')
#' }
widgetLanguage <- function(inputId, lang=list(), path=NULL) {
	init <- function() {
	  if (is.null(path)) path <- find.package('mmstat')
		pof  <- list.files(path=path, pattern="*.po$")
		pon  <- rep('', length(pof))
		pod  <- vector('list', length(pof))
		for (i in seq(pof)) {
			pon[i]   <- sapply(strsplit(pof[i], '.', fixed=T), function(elem) { elem[1] })
			msg      <- paste(readLines(paste(path, pof[i], sep="/")), collapse=" ")
			msgid    <- regmatches(msg, gregexpr('msgid\\s*".*?"', msg))
			tmp      <- strsplit(msgid[[1]], '"')
			msgid    <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
			msgstr   <- regmatches(msg, gregexpr('msgstr\\s*".*?"', msg))
			tmp      <- strsplit(msgstr[[1]], '"')
			msgstr   <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
			pod[[i]] <- list(id=msgid, str=msgstr)
		}
		names(pod) <- pon
		return(pod)
	}
	
	env               <- widget(inputId, NULL)
	# init
	env[['data']]     <- init()
	env[['selected']] <- 1
	env[['log']]      <- (Sys.getenv('SHINY_PORT')=="")
	#
	args              <- list(inputId='language', label="Choose language",
														choices=enumChoices(names(env[['data']])))
 	env[['ui']]$lang  <- list(func='selectInput',
 	                          args=mergeListsByName(args, lang))
 	env[['lang']]     <- env
 	
 	### observer
 	env[['observe']] <- expression(observe({
 		inp  <- getInputs(env)
 		env[['selected']] <- as.numeric(input[[inp[1]]])
 		envs <- c(env[['env']], env)
    # parameter processing list
    for (envi in envs) {
    	for (i in seq(length(envi[['ui']]))) {
      	func <- envi[['ui']][[i]]$func
      	id   <- envi[['ui']][[i]]$args$inputId
      	#
      	args <- list()
    	  args$inputId <- paste(envi[['widgetId']], id, sep=".")
    	  ppp          <- mmstat.env$pp[[func]]$lang
    	  if (!is.null(ppp)) {
    	  	for (k in seq(length(ppp))) {
    	  		args[[ppp[k]]] <- envi[['ui']][[i]]$args[[ppp[k]]]
    	  		if (!is.null(args[[ppp[k]]])) args[[ppp[k]]] <- getText(args[[ppp[k]]], env[['lang']])
    	  	}
    	  }
    	  ppp <- mmstat.env$pp[[func]]$langnames
    	  if (!is.null(ppp)) {
    	    for (k in seq(length(ppp)))	{
    	    	args[[ppp[k]]] <- envi[['ui']][[i]]$args[[ppp[k]]]
    	    	if (!is.null(args[[ppp[k]]])) names(args[[ppp[k]]]) <- getText(names(args[[ppp[k]]]), env[['lang']])
    	    }
    	  }
    	  if (length(args)>1) {
    	  	# Copy parameters from input if necessary
    	  	ppp  <- mmstat.env$pp[[func]]$input
    	  	if (!is.null(ppp)) {
    	  		for (k in seq(length(ppp)))	{
    	  			args[[ppp[k]]] <- isolate(input[[args$inputId]])
    	  		}
    	  	}
    	  	updt <- mmstat.env$pp[[func]]$update
    	  	args$session <- session
      	  do.call(updt, args)
        }
    	}
    }
 	}))
 	#
  return(env)
}
