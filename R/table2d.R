#' table2d
#'
#' Converts a one dimensional table to a two dimensional table.
#'
#' @param tab table: one dimensional table to convert
#' @param horizontal logical: horizontal or vertical table (default: \code{FALSE})
#' @param header character: header for the single column or row
#'
#' @return a two dimensional table
#' @export
#'
#' @examples
#' tab <- table(round(rnorm(100)))
#' tab
#' table2d(tab)
#' str(tab)
#' str(table2d(tab, horizontal=TRUE))
table2d <- function (tab, horizontal=FALSE, header=NULL) {
	if (length(dimnames(tab))!=1) stop('one dimensional table required')
	ntab <- names (tab)
	if (horizontal) {
		ret           <- as.table(matrix(tab, nrow=1))
		dimnames(ret) <- list(if(is.null(header)) '' else header, ntab)		
	} else {
		ret           <- as.table(matrix(tab, ncol=1))
		dimnames(ret) <- list(ntab, if(is.null(header)) '' else header)
	}
	ret
}