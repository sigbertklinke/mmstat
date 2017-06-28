expand_args <- function(...){
	dots <- list(...)
	max_length <- max(sapply(dots, length))
	lapply(dots, rep, length.out = max_length)
}

#' distrFunc
#' 
#' Creates a distrFunc object from a distribution name. The density/mass (\code{func='d'}), 
#' the cumulative distribution (\code{func='p'}) and the quantile distribution (\code{func='q'}) function 
#' are possible choices.
#' 
#' The standard distributions from the \pkg{stats} package (except the multinomial
#' distribution) are supported 
#' \tabular{ll}{ 
#' \code{beta}    \tab beta distribution \cr 
#' \code{binom}   \tab binomial (including Bernoulli) distribution \cr 
#' \code{cauchy}  \tab Cauchy distribution \cr 
#' \code{exp}     \tab exponential distribution \cr 
#' \code{f}       \tab F distribution \cr 
#' \code{gamma}   \tab gamma distribution \cr 
#' \code{geom}    \tab geometric distribution \cr
#' \code{hyper}   \tab hypergeometric distribution \cr 
#' \code{lnorm}   \tab log-normal distribution \cr 
#' \code{nbinom}  \tab negative binomial distribution \cr 
#' \code{norm}    \tab normal distribution \cr 
#' \code{pois}    \tab Poisson distribution \cr 
#' \code{t}       \tab Student's t distribution \cr 
#' \code{unif}    \tab uniform distribution \cr 
#' \code{weibull} \tab Weibull distribution 
#' }
#' 
#' The function computes the range of the non-zero density/mass function and the
#' maximum of the density/mass function.
#' 
#' @param type character: distribution name
#' @param func character: type of function (default: \code{'d'})
#' @param discrete logical: if distribution is discrete or not (default:
#'   \code{FALSE})
#' @param supp range: the support of the distribution function (default: \code{c(NA, NA)}, detect automatically)
#' @param xlim range: set the range of the non-zero density/mass function
#'   (default: \code{c(NA, NA)}, detect automatically)
#' @param ylim range: set the range of the y-values 
#'   (default: \code{c(NA, NA)}, detect automatically)
#' @param tol numeric: tolerance value (default:
#'   \code{.Machine$double.eps^0.25})
#' @param pretty logical: use the function \code{pretty} to determine good limits
#' @param ... further parameters depending on the distribution
#'   
#' @details For a standard distribution of the \pkg{stats} package you need only
#'   to provide the name in the parameter \code{type}. For a non-standard distribution with
#'   the name 'wwww' is expected that the following functions exist: 
#'   \tabular{ll}{
#'   \code{dwwww(x, ...)} \tab density/mass function \cr
#'   \code{pwwww(q, ...)} \tab distribution function \cr
#'   \code{qwwww(p, ...)} \tab quantile function
#'   } 
#'   A discrete distribution must be defined for non-negative integers.
#'   
#'   The range of non-zero density/mass function is computed by \code{qwwww(0,
#'   ...)} and \code{qwwww(1, ...)}. If any of these quantiles returns
#'   plus/minus infinity then \code{qwwww(tol, ...)} or \code{qwwww(1-tol, ...)}
#'   is used. Alternatively you may set the range by the parameter \code{xlim} 
#'   and a \code{NA} value implies the quantile function should be used.
#'   
#' @return a distribution object
#' @export
#' 
#' @examples
#' d <- distrFunc("norm")
#' d
distrFunc  <- function(type, ...) { UseMethod("distrFunc") }

#' @rdname distrFunc
#' @export
distrFunc.default <- function(type, func='d', discrete=F, supp=c(NA, NA), xlim=c(NA, NA), ylim=c(NA, NA),
															tol=.Machine$double.eps^0.25, pretty=T, ...) {
	n2v <- function (x, val=NA) { if (is.null(x)) val else x }
	zeroone  <- function (f, fargs, tol, max) {
		if (max) {
			fargs$p <-1
			ret <- do.call(f, fargs)
			while (is.infinite(ret)) {
  			fargs$p <- fargs$p-tol
  			ret <- do.call(f, fargs)
			}
		} else {
			fargs$p <-0
			ret <- do.call(f, fargs)
			while (is.infinite(ret)) {
				fargs$p <- fargs$p+tol
				ret <- do.call(f, fargs)
			}
		}
		return(ret)
	}
	
	dist      <- list(discrete=discrete, supp=supp, xlim=xlim, ylim=ylim)
	dist$arg  <- list(...)	
	dist$type <- switch (type,
			 		       binom   = {dist$discrete=T; dist$supp=c(0,n2v(dist$arg$size))},
								 beta    = {dist$discrete=F; dist$supp=c(0,1)},
								 cauchy  = {dist$discrete=F; dist$supp=c(NA,NA)},
								 chisq   = {dist$discrete=F; dist$supp=c(0,NA)},
								 exp     = {dist$discrete=F; dist$supp=c(0,NA)},
								 f       = {dist$discrete=F; dist$supp=c(0,NA)},
								 gamma   = {dist$discrete=F; dist$supp=c(0,NA)},
								 geom    = {dist$discrete=T; dist$supp=c(0,NA)},
								 hyper   = {dist$discrete=T; dist$supp=c(0,n2v(dist$argk))},
								 lnorm   = {dist$discrete=F; dist$supp=c(0,NA)},
								 nbinom  = {dist$discrete=T; dist$supp=c(0,NA)},
								 norm    = {dist$discrete=F; dist$supp=c(NA,NA)},
								 pois    = {dist$discrete=T; dist$supp=c(0,NA)},
								 t       = {dist$discrete=F; dist$supp=c(NA,NA)},
								 unif    = {dist$discrete=F; dist$supp=c(n2v(dist$arg$min, 0),n2v(dist$arg$max, 1))},
								 weibull = {dist$discrete=F; dist$supp=c(0, NA)},
								 NULL)
	dist$type <- type 
	dist$func <- func
	#
	quantile <- match.fun(paste0('q', type))
	if (is.na(dist$supp[1])) dist$supp[1] <- zeroone(quantile, dist$arg, tol, FALSE)
	if (is.na(dist$supp[2])) dist$supp[2] <- zeroone(quantile, dist$arg, tol, TRUE)
	if (is.na(dist$xlim[1])) {
		dist$xlim[1] <- if (func=='q') 0 else dist$supp[1]
	}
	if (is.na(dist$xlim[2])) {
		dist$xlim[2] <- if (func=='q') 1 else dist$supp[2]
	}
	if (is.na(dist$ylim[1])) {
		dist$ylim[1] <- if (func=='q') dist$supp[1] else 0
	}	
	if (is.na(dist$ylim[2])) {
		dist$ylim[2] <- if (func=='q') dist$supp[2] else 1
		if (func=='d') { # find density maximum
  		density <- match.fun(paste0('d', type))
	  	args    <- dist$arg
		  if (dist$discrete) {
		  	args$x    <- dist$supp[1]:dist$supp[2]
			  dmax      <- max(do.call(density, args))
		  } else {
			  args$f       <- density
			  args$lower   <- dist$supp[1]
			  args$upper   <- dist$supp[2]
			  args$maximum <- T
			  res          <- do.call("optimize", args)
			  dmax         <- res$objective
		  }
	  	dist$ylim[2] <- dmax
		}
	}	
	if (pretty) {
		dist$xlim <- range(pretty(dist$xlim))
		dist$ylim <- range(pretty(dist$ylim))
	}
	class(dist) <- 'distrFunc'
	dist
}

#' is.distrFunc
#'
#' The function \code{is.distrFunc} indicates whether \code{x} is a distribution function.
#' 
#' @param x an \code{R} object to be tested
#'
#' @return returns if \code{x} is a distribution function object
#' @export
#'
#' @examples
#' d <- distrFunc("norm")
#' is.distrFunc(d)
#' is.distrFunc(pi)
is.distrFunc <- function(x) { inherits(x, "distrFunc") }

#' print.distrFunc
#'
#' Prints information about a distribution function.
#'
#' @param x    distribution function object
#' @param ...  unused
#'
#' @return Returns invisibly \code{x}.
#' @export
#'
#' @examples
#' dn <- distrFunc("norm")
#' dn
#' dg <- distrFunc("geom", prob=0.7)
#' dg
print.distrFunc  <- function(x, ...) {
	cat(sprintf("Distribution type : %s\n", x$type))
	cat("Parameter(s)      :")
	str(x$arg)
	cat(sprintf("Discrete          : %s\n", if(x$discrete) "yes" else "no"))
	cat(sprintf("Function type     : %s\n", 
				                                if(x$func=='d') { if(x$discrete) "mass" else "density" } 
                                        else { if(x$func=='d') "distribution" else "quantile"}))
	cat("Function call     : ") 
  str(match.fun(paste0(x$func, x$type)))
	cat(sprintf("Plotting area     : %.2f - %.2f by %.2f - %.2f\n", x$xlim[1], x$xlim[2], x$ylim[1], x$ylim[2]))
	invisible(x)
}

#' plot.distrFunc
#' 
#' Plots a distribution function. For a discrete distribution a needle plot (mass) or an stepwise 
#' function (distributiion, quantile) is used. For a continuous distribution simple lines 
#' are used. Since internally \code{points} and \code{lines} is used, the further drawing parameters
#' relate to that. 
#' 
#' @param type distribution function object
#' @param add logical: add to current plot (default: add=F)
#' @param xlim range: plotting area (default: NULL)
#' @param ylim range: plotting area (default: NULL)
#' @param cex.lettering numeric: magnification size for fonts used (default: 1)
#' @param n numeric: number of points used to draw the curve (default: 201)
#' @param ... further drawing parameter, e.g color etc.
#'
#' @export
#'
#' @examples
#' ## Standard normal distribution (continuous)
#' # density
#' d <- distrFunc("norm")
#' plot(d)
#' # distribution
#' d <- distrFunc("norm", func="p")
#' plot(d)
#' # quantile
#' d <- distrFunc("norm", func="q")
#' plot(d)
#' ## Geoimetric distribution (continuous)
#' # density
#' d <- distrFunc("geom", prob=0.5)
#' plot(d)
#' # distribution
#' d <- distrFunc("geom", prob=0.5, func="p")
#' plot(d)
#' # quantile
#' d <- distrFunc("geom", prob=0.5, func="q")
#' plot(d)
plot.distrFunc <- function(type, add=F, xlim=NULL, ylim=NULL, cex.lettering=1, n=501, ...) {
	if (is.null(xlim)) xlim <- type$xlim
	if (is.null(ylim)) ylim <- type$ylim
	#
	if (!add) {
		pargs <- list(...)
		if (is.null(pargs$xlab)) pargs$xlab <- ''
		if (is.null(pargs$ylab)) pargs$ylab <- ''
		if (is.null(pargs$xlim)) pargs$xlim <- xlim
		if (is.null(pargs$ylim)) pargs$ylim <- ylim
		if (is.null(pargs$cex.main)) pargs$cex.main <- 1.2
		pargs$cex.main <- cex.lettering*pargs$cex.main
		if (is.null(pargs$cex.sub)) pargs$cex.sub <- 1
		pargs$cex.sub  <- cex.lettering*pargs$cex.sub
		if (is.null(pargs$cex.lab)) pargs$cex.lab <- 1
		pargs$cex.lab <- cex.lettering*pargs$cex.lab
		if (is.null(pargs$cex.axis)) pargs$cex.axis <- 1
		pargs$cex.axis <- cex.lettering*pargs$cex.axis
		pargs$x <- mean(xlim)
		pargs$y <- mean(ylim)
		pargs$type <- 'n'
		do.call('plot', pargs)
	}
	pargs <- list(...)
	usr   <- graphics::par("usr")
	if (type$discrete) {
		if (is.null(pargs$pch)) pargs$pch <- 19
		cargs <- type$arg
	  if (type$func=='d') {
	  	pargs$x <- cargs$x <- seq(xlim[1], xlim[2], by=1)
	  	call    <- match.fun(paste0(type$func, type$type))
	  	pargs$y <- do.call(call, cargs)
	  	#
	  	do.call("points", pargs)
	  	pargs$type <- 'h'
	  	do.call("lines", pargs) 
	  }
		if (type$func=='p') {
			x <- pargs$x <- cargs$q <- seq(xlim[1], xlim[2], by=1)
			call         <- match.fun(paste0(type$func, type$type))
			y <- pargs$y <- do.call(call, cargs)
			do.call("points", pargs)
			#
      pargs$x <- c(usr[1], xlim[1])
			pargs$y <- c(0,0)
			do.call("lines", pargs)
		  for (i in (1:(length(x)))) { 
			  pargs$x <- c(x[i], x[i]+1)
			  pargs$y <- c(y[i], y[i])
			  do.call("lines", pargs)
		  }
			pargs$x <- c(max(x)+1, usr[2])
			pargs$y <- c(y[length(x)], y[length(x)])
			do.call("lines", pargs)
		}
		if (type$func=='q') {
			y <- pargs$y <- cargs$q <- seq(ylim[1], ylim[2], by=1)
			f <- match.fun(paste0('p', type$type))
			x <- pargs$x <- do.call(f, cargs)
			do.call("points", pargs)
			#
			print(cbind(x,y))
			xs <- 0
			for (i in (1:length(x))) { 
				pargs$x <- c(xs, x[i])
				pargs$y <- c(y[i], y[i])
				do.call("lines", pargs)
				xs <- x[i]
			}
		}
	} else {
		cargs <- type$arg
		if (type$func=='d') {
			cargs$x <- pargs$x <- seq(xlim[1], xlim[2], length.out=n)
			call    <- match.fun(paste0(type$func, type$type))
			pargs$y <- do.call(call, cargs)
			do.call("lines", pargs)
		}			
		if (type$func=='p') {
			cargs$q <- pargs$x <- seq(xlim[1], xlim[2], length.out=n)
			call    <- match.fun(paste0(type$func, type$type))
			pargs$y <- do.call(call, cargs)
			do.call("lines", pargs)
		}		
		if (type$func=='q') {
			cargs$p <- pargs$x <- seq(xlim[1], xlim[2], length.out=n)
			call    <- match.fun(paste0(type$func, type$type))
			pargs$y <- do.call(call, cargs)
			do.call("lines", pargs)
		}		
	}
}

#' vline
#'
#' Draws one (or more) vertical line(s) in a plot. The line is drawn from the bottom until it 
#' intersects the distribution function and either does not continut (\code{cont=1}, default), 
#' continues to the left (\code{cont=2}), to the top (\code{cont=3}) or to the right (\code{cont=4}).
#'
#' @param dist distribution object
#' @param x numeric: x-coordinates 
#' @param cont numeric: continuation (default: 1)
#' @param ... further graphical parameters, e.g. line color etc.
#'
#' @return Returns invisibly a list with x-y-coordinates of the intersection of x coordinates 
#' with the distribution function.
#' @export
#'
#' @examples
#' d <- distrFunc("norm")
#' plot(d)
#' vline(d, -1.96, cont=2, col="blue")
#' vline(d, 0)
#' vline(d, +1.96, cont=4, col="blue")
vline <- function(dist, x, cont=1, ...) {
	if (!is.distrFunc(dist)) stop('A distribution function expected as first parameter')
	p     <- expand_args(x=x, cont=cont)
	pargs <- list(...)
	call  <- match.fun(paste0(dist$func, dist$type))
	cargs <- dist$arg
	cargs[[switch(dist$func, d='x', p='q', q='p')]] <- p$x 
  p$y   <- do.call(call, cargs)
  usr   <- graphics::par("usr")
  for (i in seq(length(p$x))) {
  	pargs$x <- c(p$x[i], p$x[i])
  	pargs$y <- c(usr[3], p$y[i])
		if (p$cont[i]==2) {
				pargs$x <- c(pargs$x, usr[1])
				pargs$y <- c(pargs$y, p$y[i])
		}
  	if (p$cont[i]==3) {
  		pargs$x <- c(pargs$x, x[i])
  		pargs$y <- c(pargs$y, usr[4])
  	}
  	if (p$cont[i]==4) {
  		pargs$x <- c(pargs$x, usr[2])
  		pargs$y <- c(pargs$y, p$y[i])
  	}
  	do.call("lines", pargs)
  }
  invisible(list(x=p$x, y=p$y))
}

#' interval
#'
#' Plots interval(s) according to the selected distribution function. Please note that for a continuous
#' distribution \code{polygon} is used whereas for a discrete distribution \code{points} and \code{lines}
#' is used. Therefore the further graphical parameters must be set accordingly.
#' 
#' @details 
#' Note: for the distribution and quantile function of a discrete distribution an error is generated!
#'
#' @param dist distribution function
#' @param left numeric: left interval border
#' @param right numeric: right interval border
#' @param shift numeric: shift of x in case of a mass function
#' @param n numeric: number of points in case of a continuous distribution
#' @param ... further named graphical parameters
#'
#' @export
#'
#' @examples
#' d <- distrFunc("norm")
#' plot(d)
#' interval(d, -1.96, +1.96, col="red")
#' d <- distrFunc("geom", prob=0.7)
#' plot(d)
#' interval(d, 1, 5, col="red")
interval <- function(dist, left, right, shift=0, n=201, ...) {
	if (!is.distrFunc(dist)) stop('A distribution function expected as first parameter')
	p     <- expand_args(left=left, right=right, n=n, shift=shift)
	pargs <- list(...)
	if (dist$discrete) {
		if (is.null(pargs$pch)) pargs$pch <- 19
		cargs <- dist$arg
		for (i in seq(length(p$left))) {
			if (dist$func=='d') {
				pargs$x <- cargs$x <- seq(ceiling(p$left[i]), floor(p$right[i]), by=1)
				call    <- match.fun(paste0(dist$func, dist$type))
				pargs$y <- do.call(call, cargs)
				pargs$x <- pargs$x+p$shift[i]
				#
				do.call("points", pargs)
				pargs$type <- 'h'
				do.call("lines", pargs) 
			} else {
			  stop('not defined for a discrete distribution or quantile function')	
			}
		}
	} else {
		call  <- match.fun(paste0(dist$func, dist$type))
		cargs <- dist$arg
		elem  <- switch(dist$func, d='x', p='q', q='p')
		for (i in seq(length(p$left))) {
			x <- seq(p$left[i], p$right[i], length.out=p$n[i])
      cargs[[elem]] <- x 
			y <- do.call(call, cargs)
			pargs$x <- c(p$left[i], x, p$right[i])
			pargs$y <- c(dist$ylim[1], y, dist$ylim[1])
			do.call('polygon', pargs)
		}
	}
}