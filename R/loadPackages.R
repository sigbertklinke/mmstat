#' loadPackages
#'
#' Tries to load a package with \code{require}. If it fails then installs the package via \code{install.packages} 
#' and tries to reload it again with \code{library}.
#'
#' @param libnames character: names of libraries to load
#'
#' @return boolean: if a library could be successfully loaded
#' @export
#'
#' @examples
#' loadPackages('shiny', 'shinydashboard')
loadPackages <- function (libnames, ...) {
	libnames <- c(libnames, unlist(list(...)))
  ret <- rep(T, length(libnames))
  names(ret) <- libnames
	for (libname in libnames) {
		if (!require(libname, character.only=TRUE)) {
			install.packages(libname)
			ret[libname] <- library(libname, character.only=TRUE)
		}
	}
  invisible(ret)
}