#' CI.mean
#'
#' Computes the confidence interval for the mean of the sample 
#'
#' @param sample numeric: data vector
#' @param sd numeric: if given then the standard deviation is assumed to be known (default: \code{NULL})
#' @param level numeric: level of the confidence interval() default: \code{0.95})
#' @param B numeric: if larger than zero then an bootstrapped confidence interval is computed (default: \code{0})
#'
#' @return confidence interval
#' @export
#'
#' @examples
#' x <- runif(100)
#' CI.mean(x)
#' CI.mean(x, sd=1/sqrt(12))
#' CI.mean(x, B=1000)
CI.mean <- function (sample, sd=NULL, level=0.95, B=0) {
	n <- length(sample)
	if (B>0) {
		m <- rep(NA, B)
		for (i in 1:B) m[i] <- mean(sample(sample, n, replace=T))
		res <- c(quantile(m, (1-level)/2), mean(sample), quantile(m, (1+level)/2))
	} else {
		if (is.null(sd)) {
		  sd <- sd(sample)
		  c  <- qt((1+level)/2, n-1)
	  } else {
		  c  <- qnorm((1+level)/2)
	  }
	  m <- mean(sample)
	  d <- c*sd/sqrt(n)
	  res <- c(m-d, m, m+d)
	}
	res
}

CI.median.box <- function(sample, level=0.95) {
	n   <- length(sample)
  iqr <- IQR(sample)
	m   <- median(sample)
	d   <- qnorm((1+level)/2)*0.9259259*iqr/sqrt(n)
	c(m-d, m, m+d)
}

CI.quantile <- function(sample, prob=0.5, level=0.95, B=0) {
	n <- length(s)
	l <- (1-level)/2
	u <- (1+level)/2
	if (B>0) {
		m <- rep(NA, B)
		for (i in 1:B) m[i] <- quantile(sample(sample, n, replace=T), prob)
		res <- c(quantile(m, u), quantile(sample, prob), quantile(m, l))
	} else {
		s   <- sort(sample)
		ql  <- qbinom(l, n, prob)+2
		while (pbinom(ql, n, prob)>(1-level)/2) ql<-ql-1
	  ql  <- ql+1
		pl  <- pbinom(ql-1, n, prob)
		qu  <- qbinom(u, n, prob)-2
		# ensure that CI has at least level
		while ((pbinom(qu, n, prob)-pl)<level) qu<-qu+1
		res <- rbind(res, c(s[ql+1], quantile(s, prob), s[min(n,qu+1)]))
	}
  res
}

setArgs <- function (...) {
	ddd  <- list(...)
	ddn  <- names(ddd)
	args <- list()
	for (i in 1:length(ddd)) {
		if (is.list(ddd[[i]])) {
			for (name in names(ddd[[i]])) args[[name]] <- ddd[[i]][[name]]
		} else {
			if (is.null(ddn) || (ddn[[i]]=='')) stop('Unnamed non-list elements are not allowed')
			args[[ddn[i]]] <- ddd[[i]]
		}
	}
	args
}

#' confidenceInterval
#'
#' Creates confidence intervals for a single parameter
#'
#' @param x numeric: vector of population values
#' @param CI function: computes a confidence interval, see details
#' @param param numeric: true parameter from the population if known (default: \code{NULL})
#' @param sample numeric: number of samples to compute (default: \code{25})
#' @param size numeric: sample sizes to use for the confidence interval computation (default: \code{length(x)/2})
#' @param discrete logical: if population is discrete (default: \code{FALSE})
#' @param replace logical: draw sample with replacement or not (default: \code{TRUE})
#' @param ... further parameters
#'
#' @details ...
#'
#' @rdname confidenceInterval
#' @export
#'
#' @examples
#' ci <- confidenceInterval(runif(100), CI=CI.mean)
confidenceInterval  <- function(x, ...) { UseMethod("confidenceInterval") }

#' @return \code{NULL}
#'
#' @rdname confidenceInterval
#' @method confidenceInterval default
#' @export
#' 
#' 
confidenceInterval.default <- function(x, CI, param=NULL, sample=25, size=length(x)/2, 
																			 discrete=FALSE, replace=TRUE,  ...)  {
	samp <- list()
	n    <- length(x)
	for (i in 1:sample) {
		index <- sample(n, size, replace=replace)
		samp[[i]] <- list(index=index, ci=CI(x[index], ...))
	}
	ci <- list(pop=x, y=runif(n), samp=samp, disc=discrete, param=param)
	class(ci) <- 'confidenceInterval'
	ci
}


#' plot.confidenceInterval
#'
#' @param x confidence interval object
#' @param population list: parameters to draw the population points
#' @param sample list: parameters to draw the sample points
#' @param ci.lines list: parameters to draw the confidence interval
#' @param ci.points list: parameters to draw the estimated parameter
#' @param param.abline list: parameters to draw the line of the true parameter  
#' @param text.population character: text to write for the population 
#' @param ... further parameters given \code{\link[graphics]{plot}}
#'
#' @return plot
#' @import graphics
#' @export
#'
#' @examples
#' x  <- runif(100) # generate some population
#' ci <- confidenceInterval(x, CI.mean)
#' plot(ci)
plot.confidenceInterval <- function(x, 
																		population=list(pch=19, cex=0.5, col=getOption('mmstat.col.population')[1]), 
																		sample=list(pch=19, cex=0.25, col=getOption('mmstat.col.sample')[1]), 
																		ci.lines=list(lwd=1, col=c("black", "red")), 
																		ci.points=list(pch=19, cex=0.5, col=c("black", "red")),
																		param.abline=list(col="red"),
																		text.population="Population",
																		...) {
  nsamp <- length(x$samp)
  args  <- setArgs(ylab="Sample",
  								 xlab="",
  								 list(...), 
  								 xlim = range(x$pop), 
  								 ylim = if (!is.null(population)) c(-2, nsamp) else c(0, nsamp),
  								 axes = FALSE,
  								 type = 'n',
                   x    = mean(range(x$pop))
  )
  do.call('plot', args)
  axis(1)
  ax     <- axTicks(2)
  ax     <- c(1, ax[ax>0])
  labels <- sprintf("%i", ax)
  if (!is.null(population)) {
  	ax <- c(-1, ax)
  	labels <- c(text.population, labels)
  }
  axis(2, at=ax, labels=labels)
  box()
  
  ci.lc <- ci.lines$col
  ci.pc <- ci.points$col
  for (i in 1:nsamp) {
  	if (!is.null(sample)) {
  		sample$x <- x$pop[x$samp[[i]]$index]
  	  sample$y <- i-0.1-0.4*x$y[x$samp[[i]]$index]
  		do.call('points', sample)
  	}
  	ci.lines$x   <- x$samp[[i]]$ci
  	ci.lines$y   <- c(i,i,i)
  	cind <- if (!is.null(x$param) && ((x$param<min(x$samp[[i]]$ci)) || (x$param>max(x$samp[[i]]$ci)))) 2 else 1 
  	ci.lines$col <- ci.lc[cind]
  	do.call('lines', ci.lines)
  	ci.points$x   <- x$samp[[i]]$ci[2]
  	ci.points$y   <- i
  	ci.points$col <- ci.pc[cind]
  	do.call('points', ci.points)
  }
  if (!is.null(population)) {
  	population$x <- x$pop
  	population$y <- x$y-1.5
  	do.call('points', population)
  }
  if (!is.null(x$param)) {
  	param.abline$v <- x$param
  	do.call('abline', param.abline)
  }
}