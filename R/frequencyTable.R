#' frequencyTable
#'
#' @param obj numeric: a vector of numeric values to group
#' @param ... currently unused
#'
#' @rdname frequencyTable
#' @export
#'
#' @examples
#' ft <- frequencyTable(rnorm(100))
frequencyTable <- function (obj, ...) { UseMethod("frequencyTable") }

#' @param breaks numeric: breaks for creating a frequency table
#'
#' @return \code{NULL}
#'
#' @rdname frequencyTable
#' @method frequencyTable default
#' @export 
frequencyTable.default <- function (obj, breaks=5, ...) {
	ret         <- list(x=obj)
	ret$breaks  <- if(length(breaks)==1) pretty(obj, n=breaks) else breaks
	lb          <- length(ret$breaks)
	ret$lower   <- ret$breaks[1:(lb-1)]
	ret$upper   <- ret$breaks[2:lb]
	ret$mids    <- (ret$lower+ret$upper)/2
	ret$width   <- ret$upper-ret$lower
	ret$cut     <- findInterval(obj, ret$breaks)
  ret$counts  <- table(factor(ret$cut, levels=1:(lb-1)))
  ret$freq    <- ret$counts/sum(ret$counts)
  ret$density <- ret$freq/diff(ret$breaks)
  ret$ecdf    <- cumsum(ret$freq)
  class(ret)  <- c("frequencyTable")
  ret
}


#' generateObject.frequencyTable
#'
#' @param obj frequency table object
#' @param digits numeric: number of significant digits for relative frequency (default: \code{2})
#' @param n range: range of observation numbers
#' @param zero logical: are lasses with zero observations allowed (default: \code{TRUE}) 
#' @param ... currently unused
#'
#' @return a frequency table object
#' @export
#'
#' @examples
#' ft <- frequencyTable(runif(100))
#' ft
#' ft2 <- generate(ft)
#' ft2
generateObject.frequencyTable <- function(obj, digits=2, n=c(0.75, 1.25)*length(obj$x), zero=TRUE, ...) {
  # adjustCounts
	ns <- floor(min(n)):ceiling(max(n))
	f  <- (0:10^digits)/10^digits 
  dev    <- Inf
  countm <- obj$counts
  for (i in 1:length(ns)) {
    h  <- f*ns[i]
    h  <- h[trunc(h)==h]
    ht <- trunc(obj$counts/h[2])
    hf <- obj$counts/h[2]-ht
    nh <- ns[i]/h[2]
    while(sum(ht)<nh) {
    	pos <- which.max(hf)
    	ht[pos] <- ht[pos]+1
    	hf[pos] <- -1
    }
    devi <- sum((ht*h[2]-obj$counts)^2)
    if (!zero && any(ht==0)) devi <- Inf
    if (devi<dev) {
    	dev    <- devi
    	countm <- ht*h[2]
    }
	}
  if (is.infinite(dev)) { # no approximation found
  	warning("No frequencyTable generated, maybe you should set 'zero=TRUE'")
  	return(NULL)
  }
  # adjustX
  x     <- c()
  count <- obj$counts
  nc    <- length(countm)
  for (j in 1:nc) {
  	xj <- obj$x[as.numeric(obj$cut)==j]
  	if (countm[j]>0) {
  	  if (countm[j]==count[j]) x <- c(x, xj)
  	  if (countm[j]>count[j]) {
  		  if (length(xj)) {
  			  x <- c(x, xj, sample(xj, countm[j]-count[j], replace=T))
  		  } else {
  			  x <- c(x, obj$lower[j]+obj$width[j]*runif(countm[j]))
  		  }
  	  }
  	  if (countm[j]<count[j]) {
  		  if (length(xj)>1) {
  			  x <- c(x, sample(xj, countm[j])) 
  		  } else {
  			  x <- rep(xj, countm[j])
  		  }
  	  }
  	}
  }
  frequencyTable(x, breaks=obj$breaks)
}


#' plot.frequencyTable
#'
#' Plots a frequency table as histogram.
#'
#' @param x frequency table object
#' @param ... further parameters used \code{\link[graphics]{hist}}
#'
#' @return plot
#' @export
#'
#' @examples
#' ft <- frequenyTable(runif(100))
#' plot(ft)
plot.frequencyTable <- function(x, ...) {
	hist(x$x, breaks=x$breaks, ...)
}

#' median.frequencyTable 
#'
#' Computes the median based on the frequency table.
#'
#' @param x frequency table object
#' @param na.rm logical: unused
#' @param ... further parameters
#'
#' @return median
#' @method median frequencyTable
#' @export
#'
#' @examples
#' x  <- runif(100)
#' median(x)
#' ft <- frequencyTable(x)
#' median(ft)
median.frequencyTable <- function(x, na.rm = FALSE, ...) {
	ecdf <- c(0, x$ecdf)
	mcl  <- sum(ecdf<0.5)
	x$lower[mcl]+(0.5-ecdf[mcl])/x$freq[mcl]*x$width[mcl]
}