#' contingencyTable
#'
#' Creates a "random" contingency table of size \code{ncol} times \code{nrow} which has a total number of 
#' \code{n} observations. For \code{n} a range can be given; if a single value is given than the range 
#' \code{round(n*0.8)} till \code{round(n*1.2)} is used. With \code{digits} the number of digits after the 
#' comma for the expected frequencies can be given.
#' With the following parameter(s) a target range of association can be given for the contingency table
#' \describe{
#' \item{\code{CC}}{target range for the corrected contingency coefficient}
#' \item{\code{V}}{target range for Cramer's V}
#' \item{\code{C}}{target range for the contingency coefficient}
#' \item{\code{chisq}}{target range for the chi square value}
#' }
#' For any parameter (except \code{chisq}) given as a single value the range is computed as parameter plus/minus 0.05.
#' If \code{chisq}) is given as a single value the range is computed as parameter 
#' plus/minus \eqn{\sqrt{(ncol-1)(nrow-1)})}.
#' If more than one target parameter is given then a join of the corresponding chi square ranges will be made. 
#' 
#' @note It is not guaranteed that for all parameters combinations a solution can be found. The function stops
#' after one thousand iterations to find an appropriate table of expected or observed frequencies with an error! 
#' You may increase/enlarge the range of \code{n}, \code{cc} or increase \code{digits}.
#' 
#' @param tab a contingency table 
#' 
#' @return a contingency table of observed frequencies
#' @import stats DescTools
#' @export
#'
#' @examples
#' ct <- contingencyTable(HairEyeColor[,,"Female"])
contingencyTable <- function (tab) {
	ret            <- list(observed=tab)
	n              <- sum(tab)
	ret$relative   <- tab/n
	ret$expected   <- as.table(outer(rowSums(tab), colSums(tab))/n)
	names(dimnames(ret$expected)) <- names(dimnames(tab))
	ret$residuals  <- (ret$observed-ret$expected)/sqrt(ret$expected)
	ret$stdres     <- ret$residuals/sqrt(outer(1-rowSums(tab)/n, 1-colSums(tab)/n))
	ret$chisq      <- sum(ret$residuals^2)
	ret$C          <- sqrt(ret$chisq/(ret$chisq+n))
	k              <- min(dim(tab))
	ret$CC         <- ret$C*sqrt(k/(k-1))
	ret$V          <- sqrt(ret$chisq/n/(k-1))
	ret$lambda.cr  <- Lambda(tab, direction='column')
	ret$lambda.rc  <- Lambda(tab, direction='row')
	ret$lambda.sym <- Lambda(tab, direction='symmetric')
	ret$gkt.rc     <- GoodmanKruskalTau(tab, direction='row')
	ret$gkt.cr     <- GoodmanKruskalTau(tab, direction='column')
	ret$theil.rc   <- UncertCoef(tab, direction='row')
	ret$theil.cr   <- UncertCoef(tab, direction='row')
	ret$theil.sym  <- UncertCoef(tab, direction='symmetric')
	class(ret) <- 'contingencyTable'
	ret
}

#' coefficients
#' 
#' Extracts all requested association coefficients as two dimensional table. Possible names are:
#' 
#' \describe{
#' \item{\code{chisq}}{Chi square value}
#' \item{\code{C}}{Contingency value}
#' \item{\code{CC}}{Corrected c ontingency value}
#' \item{\code{V}}{Cramers V or phi}
#' \item{\code{lambda}}{All three Goodman and Kruskal lambdas}
#' \item{\code{lambda.cr}}{Goodman and Kruskal lambda, column dependent}
#' \item{\code{lambda.rc}}{Goodman and Kruskal lambda, row dependent}
#' \item{\code{lambda.sym}}{Goodman and Kruskal lambda, symmetric}
#' \item{\code{gkt}}{Both Goodman and Kruskal taus}
#' \item{\code{gkt.cr}}{Goodman and Kruskal tau, column dependent}
#' \item{\code{gkt.rc}}{Goodman and Kruskal tau, row dependent}
#' \item{\code{theil}}{All three uncertainty coefficients/Theil's U}
#' \item{\code{theil.cr}}{Uncertainty coefficient/Theil's U, column dependent}
#' \item{\code{theil.rc}}{Uncertainty coefficient/Theil's U, row dependent}
#' \item{\code{theil.sym}}{Uncertainty coefficient/Theil's U, symmetric}
#' }
#'
#' @param ft contingency table object
#' @param ... list: coefficients in the table
#'
#' @return a two dimensional table
#' @export
#'
#' @examples
#' ct <- contingencyTable(HairEyeColor[,,"Female"])
#' coefficients(ct, lambda=TRUE)
association <- function (ft, ...) {
	args   <- list(...)
	coeffs <- c('C'          = 'CHISQUARE.C', 
							'CC'         = 'CHISQUARE.CC',
							'chisq'      = 'CHISQUARE.VAL',
							'V'          = 'CHISQUARE.V', 
							'lambda.cr'  = 'GKLAMBDA.CR', 
							'lambda.rc'  = 'GKLAMBDA.RC', 
							'lambda.sym' = 'GKLAMBDA.SYM', 
							'gkt.rc'     = 'GKTAU.RC', 
							'gkt.cr'     = 'GKTAU.CR',  
							'theil.rc'   = 'THEILU.RC', 
							'theil.cr'   = 'THEILU.CR', 
							'theil.sym'  = 'THEILU.SYM')
	coeffl <- c('lambda', 'gkt', 'theil')
	ret    <- c()
	for (coef in names(coeffs)) {
		if (!is.null(args[[coef]]) && args[[coef]]) {
			ret[coeffs[coef]] <- ft[[coef]] 
		}
	}
	for (coef in coeffl) {
		if (!is.null(args[[coef]]) && args[[coef]]) {
			tf <- startsWith(names(coeffs), coef)
			for (coeftf in names(coeffs)[tf]) {
				ret[coeffs[coeftf]] <- ft[[coeftf]] 
			}
		}
	}
	if (length(ret)==0) return(NULL)
	nret          <- names(ret)
	ret           <- as.table(matrix(ret), ncol=1)
  dimnames(ret) <- list(nret, 'Coefficient value(s)')
	ret
}
									
#' generateObject.contingencyTable
#'
#' Create a contingency table with certain properties.
#'
#' @param obj contingency table object
#' @param n integer range: total number of observations
#' @param CC numeric range: target corrected contingency coefficient (default: c(0.45, 0.55))
#' @param V numeric range: target Cramer's V (default: NULL)
#' @param C numeric range: target contingency coefficient (default: NULL)
#' @param chisq numeric range: target chi square value (default: NULL)
#' @param digits integer: number of digits after the comma for the expected frequencies (default: 0)
#' @param verbose integer: verbosity of function (default: 0)		
#'
#' @return a contingency table with the approximately required properties
#' 
#' @method generateObject contingencyTable
#' @export generateObject.contingencyTable
#'
#' @examples
#' ft  <- contingencyTable(table(round(runif(10)), round(runif(10))))
#' ft2 <- generate(ft)
generateObject.contingencyTable <- function (obj, n=NULL, 
																						 CC=c(0.45, 0.55), V=NULL, C=NULL, chisq=NULL, 
																						 digits=0, verbose=F) {
	findTableExpected <- function (n, ncol, nrow, digits=0, maxit=1000, verbose=F) {
		it <- 0
		if (verbose>1) cat("Candidate(s) for expected frequencies\n")
		repeat {
			repeat {
  			p  <- runif(nrow)
	  		nc <- round(outer(n, p/sum(p), "*"))
		  	sc <- rowSums(nc)
			  p  <- runif(ncol)
			  nr <- round(outer(n, p/sum(p), "*")) 
			  sr <- rowSums(nr)    
			  if (all(nc>0, nr>0)) break
			}
			for (i in 1:nrow(nc)) {
				for (j in 1:nrow(nr)) {
					if (sc[i]==sr[j]) { # row and col sums match
						expected <- outer(nc[i,], nr[j,], function(ni, nj, s) { ni*nj/s }, s=sc[i])	
						if (verbose>1) print(expected)
						if (all(round(expected, digits)==expected)) return(expected)
					}
				}
			}
			it <- it+1
			if (it>maxit) stop ('no table of expected frequencies found')
		}	
	}
	#
	findTableObserved <- function (expected, chisq, maxit=1000, verbose=0) {
		it <- 0
		observed <- expected
		if (verbose>1) cat("Candidate(s) for observed frequencies\n")
		chisqt    <- sum((observed-expected)^2/expected)
		if ((chisq[1]<=chisqt) & (chisqt<=chisq[2])) return(expected)
		repeat {
			candidate         <- observed
			pos               <- sample(length(observed), 2)
      candidate[pos[1]] <- observed[pos[1]]+1
	    candidate[pos[2]] <- observed[pos[2]]-1
      if (verbose>1) print(candidate)
	    if (validTable(candidate)) {
  			chisqc <- sum((candidate-expected)^2/expected)
  			if (chisqc<=chisq[2]) {
	  		  if (chisq[1]<=chisqc) return(candidate)
		  	  if (chisqc>chisqt) {
			  	  observed <- candidate
				    chisqt    <- chisqc			
		  	  }
  			}
	    }
			it <- it+1
			if (it>maxit) stop ('no table of observed frequencies found')
		}
	}
	#
	validTable <- function (tab) {
		all(tab>-1, rowSums(tab)>0, colSums(tab)>0)
	}
	#
	makeRange <- function(coeff, p=0.05, min=0, max=1) {
		cmin <- min(coeff)
		cmax <- max(coeff)
		if (cmax==cmin) {
			cmin <- cmin-p
			cmax <- cmax+p
		}
		if (cmin<min) cmin <- min
		if (cmax>max) cmax <- max
		return(c(cmin,cmax))
	}
	#
	ncol <- ncol(obj$observed)
	nrow <- nrow(obj$observed)
	if (!is.null(n)) { 
	  nmin <- as.integer(min(n))
	  nmax <- as.integer(max(n))
	} else {
		nmin <- nmax <- sum(obj$observed)
	}
	if (nmin==nmax) {
		nmin <- round(nmin*0.8)
		nmax <- round(nmax*1.2)
	}
	if (verbose>0) {
		cat("Target n\n")
		print(c(nmin, nmax))
	}
	chisq <- NA
	if (!is.null(CC)) { # corrected contingency coefficient 
		cc    <- makeRange(CC)
		if (verbose>0) {
			cat("Target CC\n")
			print(cc)
		}
		c     <- (min(ncol,nrow)-1)/min(ncol,nrow)*cc^2
		chisq <- range(outer(nmin:nmax, c, function(ni, cj) { ni*cj/(1-cj) }), chisq, na.rm=T)
	}	
	if (!is.null(C)) { # contingency coefficient
		c     <- makeRange(C)
		if (verbose>0) {
			cat("Target C\n")
			print(c)
		}
		chisq <- range(outer(nmin:nmax, c, function(ni, cj) { ni*cj/(1-cj) }), chisq, na.rm=T)
	}
	if (!is.null(V)) { # contingency coefficient
		v     <- makeRange(V)
		if (verbose>0) {
			cat("Target Cramers V\n")
			print(v)
		}
		v <- v^2*min(ncol-1, nrow-1)
		chisq <- range(outer(nmin:nmax, v, function(ni, vj) { vj*ni }), chisq, na.rm=T)
	}
	if (!is.null(chisq)) { # chi square value
		c2    <- makeRange(chisq, p=sqrt((ncol-1)*(nrow-1)), min=0, max=Inf)
		if (verbose>0) {
			cat("Target Chi square\n")
			print(range(c2))
		}
		chisq <- range(c2, chisq, na.rm=T)
	}
	#
	expected <- findTableExpected(nmin:nmax, ncol, nrow, verbose=verbose)
	if (verbose>0) {
		cat("Expected frequencies\n")
		print(expected)
	}
	# break down corrected coefficient range to chi^2 range
	observed <- findTableObserved(expected, chisq, verbose=verbose)
	# verbose
  if (verbose>0) {
  	cat("Observed frequencies\n")
  	print(observed)
  	cat("Coefficients\n")
  	chisq <- sum((observed-expected)^2/expected)
  	c    <- sqrt(chisq/(chisq+sum(expected)))
  	m    <- min(ncol(expected), nrow(expected))
  	print(c(n=sum(expected), chisq=chisq, C=c, CC=c*sqrt(m/(m-1)), V=sqrt(chisq/sum(expected)/(m-1))))
  }
  contingencyTable(observed)
}