#' Converts a \code{choices} parameter to a named list
#'
#' The resulting named list contains integer values. The advantage is 
#' that the integer values are language independent. Only the list names will be
#' checked send through \code{getText}.
#'  
#' The parameter \code{select} allows to select only a part the current list.
#'
#' @param ... choices as text vector, single texts or list
#' @param select boolean vector with the same length as \code{...}
#'
#' @return a named list
#' @export
#'
#' @examples
#' # Note: here the resulting list contains the integer values
#' enumChoices("Data set 1", "Data set 2", "Data set 3")
#' enumChoices(c("Data set 1", "Data set 2", "Data set 3"))
#' enumChoices(list("Data set 1", "Data set 2", "Data set 3"))
enumChoices <- function(..., select=NULL) {
	args <- unlist(list(...))
	ret  <- as.list(seq(length(args)))
	names(ret) <- args
	if (!is.null(select)) {
		if (length(select)!=length(ret)) stop('enumChoices: Length of choices unequal to length of select')
		ret[!select] <- NULL
	}
	ret
#	if (is.null(names(txt))) {
#		ret <- as.list(seq(length(txt)))
#		names(ret) <- txt
#	} else {
#		ret <- as.list(txt)
#		names(ret) <- names(txt)
#	}
#	if (!is.null(select)) ret[!select] <- NULL
#	ret
}

