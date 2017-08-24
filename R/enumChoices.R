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

