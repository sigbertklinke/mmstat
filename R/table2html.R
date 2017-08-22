tag <- function (name, ..., empty=FALSE) {
	if (empty) return(NULL)
	args  <- list(...)
	args[sapply(args, is.null)] <- NULL
	for (i in 1:length(args)) { 
		if (length(args[[i]])>1) args[[i]] <- paste0(args[[i]], collapse='')
	}
	nargs <- names(args)
	fargs <- nargs[nargs!='']
	ret   <- ''
	if (length(fargs)) {
		ret   <- paste0(' ', paste0(fargs, '="', args[fargs], '"', collapse=' '))
		args[fargs] <- NULL
	}
	paste0('<', name, ret, '>', paste0(args, collapse=''), '</', name, '>')
}

#' table2html
#'
#' Creates an HTML view on a two dimensional table.
#'
#' @param tab table: table to show
#' @param colsums character: If non-NULL the column sums will be computed and the contents of \code{colsums} appears as header (default: \code{NULL})
#' @param rowsums character: If non-NULL the rows sums will be computed and the contents of \code{rowsums} appears as header (default: \code{NULL})
#' @param bg colors: background color for table rows, will be recycled (default: \code{c("white", "lightGray")})
#' @param border color: background color for header cells (default: \code{"grey"})
#' @param style character: style information for the table (default: \code{"width:95\%;margin:10px;"})
#' @param ... further parameters used for the \code{\link{format}} command for formatting table entries
#' @details In case of a non-numeric table \code{colsums} and \code{rowsums} is ignored. 
#' 
#' @return character: the HTML code for the table
#' @export
#'
#' @examples
#' tab <- HairEyeColor[,,'Female']
#' htmltab <- table2html(tab, colsums='', rowsums='')
#' sink('HairEye.html')
#' cat(htmltab)
#' sink()
table2html <- function(tab, colsums=NULL, rowsums=NULL, bg=c("white", "lightGray"), border="grey", style="width:95%;margin:10px;", ...) {
	if (!("table" %in% class(tab))) stop('table required')
	labs <- names(dimnames(tab))
	if (length(labs)!=2) stop('two dimensional table required')
	collab <- if(labs[2]=='') NULL else labs[2]
	rowlab <- if(labs[1]=='') NULL else labs[1]
	lcols  <- sum(!is.null(rownames(tab)), !is.null(rowlab))
	rcols  <- as.integer(!is.null(rowsums))
	rname  <- rownames(tab)
	cname  <- colnames(tab)
	tabnn  <- col <- row <- NULL
	if (is.numeric(tab)) {
		if (!is.null(colsums)) tab <- rbind(tab, colSums(tab))
		if (!is.null(rowsums)) tab <- cbind(tab, rowSums(tab))
		tab <- format(tab, ...)
		if (!is.null(rowsums) && !is.null(colsums)) tabnn <- tab[nrow(tab), ncol(tab)] 
		if (!is.null(rowsums)) { col <- tab[,ncol(tab)]; tab <- tab[,-ncol(tab)] }
		if (!is.null(colsums)) { row <- tab[nrow(tab),]; tab <- tab[-nrow(tab),] }
		align <- 'right'
	} else {
		align <- 'left'
		tab   <- matrix(as.character(tab), ncol=ncol(tab))
	}
	rownames(tab) <- rname
	colnames(tab) <- cname
	columnlabel <- tag('tr', empty=is.null(collab), style=sprintf("background-color:%s", border), 
										 tag('th', empty=!lcols,     colspan=lcols, '&nbsp'),
										 tag('th', empty=!ncol(tab), colspan=ncol(tab), style="text-align:center;", collab),
										 tag('th', empty=!rcols,     colspan=rcols, '&nbsp')
	)
	columnname  <- tag('tr', empty=is.null(colnames(tab)), style=sprintf("background-color:%s", border), 
										 tag('th', empty=!lcols,     colspan=lcols, '&nbsp'),
										 sapply(colnames(tab), function(cname) { tag('th', style=sprintf("text-align:%s;", align), cname) }),
										 tag('th', empty=!rcols,     colspan=rcols, style="text-align:right;",
										 		       if(is.null(rowsums)) '&nbsp;' else as.character(rowsums))
	)
	firstrow    <- tag('tr', style=sprintf("background-color:%s", bg[1]),
										 tag('th', empty=is.null(rowlab), style=sprintf("background-color:%s", border), rowspan=nrow(tab), rowlab),
										 tag('th', empty=is.null(rownames(tab)),  style=sprintf("text-align:center;background-color:%s", border), rownames(tab)[1]),
										 sapply(tab[1,], function(elem) { tag('td', align=align, elem) }),
										 tag('th', empty=!rcols, colspan=rcols, style=sprintf("text-align:right;background-color:%s", border),  col[1])
	)
	furtherrows <- sapply(2:nrow(tab), function(i) {
		tag('tr', style=sprintf("background-color:%s", bg[1+((i-1)%%length(bg))]),
				tag('th', empty=is.null(rownames(tab)),  style=sprintf("text-align:center;background-color:%s", border), rownames(tab)[i]),
				sapply(tab[i,], function(elem) { tag('td', align=align, elem) }),
				tag('th', empty=!rcols, colspan=rcols, align=align, style=sprintf("text-align:right;background-color:%s", border), col[i])
		)
	})
	lastrow     <- tag('tr', empty=is.null(row), style=sprintf("background-color:%s", border), 
										 tag('th', empty=!lcols,   colspan=lcols, if(is.null(colsums)) '&nbsp;' else as.character(colsums)),
										 sapply(row, function(elem) { tag('th', style=sprintf("text-align:%s;", align), elem) }),
										 tag('th', empty=!rcols,   style=sprintf("text-align:%s;", align), colspan=rcols, tabnn)
	)
	tag('table', style=style, columnlabel, columnname, firstrow, furtherrows, lastrow)
}
