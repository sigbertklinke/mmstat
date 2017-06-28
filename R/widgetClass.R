#' widget
#'
#' Creates a new widget. Basically a widget is an \code{\link{environment}} with several components
#' 
#' \describe{
#' \item{\code{ui$elem}}{shiny UI element with (fixed) name \code{elem}}
#' \item{\code{ui$elem$func}}{shiny UI widget, e.g. \code{\link{sliderInput}}, ...}
#' \item{\code{ui$elem$args}}{\code{\link{do.call}} argument list for shiny UI widget func}
#' \item{\code{change$elem}}{store current value of \code{ui$elem} (intended for buttons)}
#' \item{\code{value$elem}}{store current value of \code{ui$elem}}
#' \item{\code{selected}}{store current number of selected language}
#' \item{\code{widgetId}}{name of widget}
#' \item{\code{observe}}{expression which contains code for observe}
#' \item{\code{lang}}{language widget (necessary for translation)}
#' \item{\code{data}}{internal data the widget needs}
#' }
#' @param inputId widget name
#' @param lang language widget
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' # empty widget with no UI elements
#' w <- widget('test', NULL)
widget <- function (inputId, lang) {
  env <- new.env()
  env[['widgetId']] <- inputId
  env[['lang']]     <- lang
  class(env)    <- c('widget', class(env))
  lang[['env']] <- if (is.null(lang[['env']])) list(env) else c(lang[['env']], env)
  return(env)
}

#' is.widget
#'
#' Tests if object \code{x} is a widget
#'
#' @param x object to be tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is a widget or not.
#' @export
#'
#' @examples
#' w <- widgetHypergeometric('test')
#' is.widget(w)
#' is.widget("no widget")
is.widget <- function(x) { inherits(x, "widget") }

#' str
#'
#' Compactly display the internal structure of a widget
#'
#' @param w widget
#'
#' @export
#'
#' @examples
#' w <- widgetHypergeometric('test')
#' str(w)
str.widget <- function(w) {
	l   <- list()
	for (obj in ls(w)) l[[obj]] <- w[[obj]]
	str(l)
}

#' print
#'
#' Prints the used Shiny UI elements and its parameter. Returns it invisibly (via \code{\link{invisible}(w)}).
#'
#' @param w widget
#'
#' @return the widget
#' @export
#'
#' @examples
#' w <- widgetHypergeometric('test')
#' print(w)
print.widget <- function(w) {
	ui <- w$ui
	if (length(ui)) {
  	for (elem in ui) {
	  	arglist <- elem$args
		  arglist$inputId <- paste(w$widgetId, arglist$inputId, sep=".")
		  delim   <- ifelse(sapply(arglist, is.character), '"', "")
 		  arglist <- paste0(names(arglist),"=", delim, as.character(arglist), delim, collapse=", ")
		  cat(sprintf("%s(%s)\n", elem$func, arglist))
  	}
  } else {
  	cat("Empty widget\n")
	}
	invisible(w)
}

#' summary
#'
#' Summarizes the widget contents
#'
#' @param w widget
#'
#' @return Informations about the widget
#' Widget name    : widget name
#' UI element(s)  : list of Shiny UI elements
#' Observe values : number of values which are observed
#' Observe changes: number of values which changes are observed
#' Observer       : does the widget has an observer
#' Language       : does the widget contain a language element (for internationalization)
#' Internal data  : does the widget contain internal data 
#' @export
#'
#' @examples
#' w <- widgetHypergeometric('test')
#' summary(w)
summary.widget <- function(w) {
	cat("Widget name    :", w$widgetId, "\n")
	cat("UI element(s)  :", paste0(lapply(w$ui, function(e) { as.character(e$func) }), collapse=", "), "\n")  
	cat("Observe values :", length(w$value), "\n")
	cat("Observe changes:", length(w$change), "\n")
	cat("Observer       :", !is.null(w[['observe']]), "\n")
	cat("Language       :", !is.null(w[['lang']]), "\n")
	cat("Internal data  :", !is.null(w[['data']]), "\n")
}

# mergable environment objects
# ui$elem       shiny ui element with fixed name
# ui$elem$func  shiny UI widget, e.g. sliderInput, ...
# change$elem   store current value of ui$elem (intended for buttons)
# value$elem    store current value of ui$elem
# observe       expression which contains code for observe

#' includeWidget
#'
#' Includes the content of the \code{src} widget into the widget \code{dest}. 
#' Elements which are not included are: language widget and internal data.
#'
#' @param dest widget to be added to
#' @param src widget to be added from 
#'
#' @export
#'
#' @examples
#' w1 <- widgetHypergeometric('hyper')
#' w2 <- widgetBinomial('binomial')
#' includeWidget(w1, w2)
#' summary(w1)
includeWidget <- function(dest, src) {
	stopif(!is.widget(dest) || !is.widget(dest), 'addWidget: try to merge non widget object(s)')
  for (obj in names(src$ui)) {
    stopif(!is.null(dest$ui[[obj]]), 'addWidget: UI element ', obj, ' already exists')
  	dest$ui[[obj]] <- src$ui[[obj]]
  	if (!is.null(src$change[[obj]])) dest$change[[obj]] <- src$change[[obj]]
  	if (!is.null(src$value[[obj]]))  dest$value[[obj]] <- src$value[[obj]]
  }
	if (!is.null(src$observe)) dest$observe <- c(dest$observe, src$observe)
}