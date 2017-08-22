library("shiny")
library("shinydashboard")
library("mmstat")

wLang  <- widgetLanguage('lang')
wFont  <- widgetFontSize('font', lang=wLang)
wDist  <- widgetSelect(list(inputId='dist',
													  label="Select a distribution type",
														choices=enumChoices(c("Binomial distribution",
																			          "Hypergeometric distribution",
																			          "Poisson distribution"))),
											 lang=wLang)
wBinom <- widgetBinomial('binom', lang=wLang)
wHyper <- widgetHypergeometric('hyper', M=list(value=10), n=list(value=5), lang=wLang)
wPois  <- widgetIntensity('pois', lang=wLang)
wPCDF  <- widgetPCDF('pcdf', lang=wLang)

mmstat <- new.env()

makePlot <- function(dist, binom, hyper, pois, pcdf, refit, cex) {
	cdf <- function(x, height) {
		n <- length(x)
		stopif(n<1, "'x' must have 1 or more non-missing values")
		rval <- approxfun(x, height,  method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
		class(rval) <- c("ecdf", "stepfun", class(rval))
		rval
	}
	#
	merge <- function(pusr, refit) {
		if (!refit) {
			usr <- mmstat$usr
			if (!is.null(usr)) {
  		  pusr[1] <- min(usr[1], pusr[1])
	  		pusr[2] <- max(usr[2], pusr[2])
		  	pusr[3] <- min(usr[3], pusr[3])
			  pusr[4] <- max(usr[4], pusr[4])
			}
		}
		mmstat$usr <- pusr
		pusr
	}
#	str(dist)
#	str(binom)
#	str(pois)
#	str(hyper)
#	str(pcdf)
#	str(refit)
#	str(cex)
	if (anyUndefined(dist, binom, hyper, pois, pcdf, refit, cex)) return(NULL)
	#
	x <- switch(dist, 
							'1'=0:binom$size, 
							'2'=0:hyper$n, 
							'3'=0:ceiling(6*pois$lambda))
	if (pcdf$pcfunc==1) { 
		if (dist==1) {
		  y    <- dbinom(x, size=binom$size, prob=binom$prob)
		  fmt  <- getText('Probability mass function of B(%i; %.2f)', wLang)
		  main <- sprintf(fmt, binom$size, binom$prob)
		}
		if (dist==2) {
			y    <- dhyper(x, m=hyper$M, n=hyper$N-hyper$M, k=hyper$n)
			fmt  <- getText('Probability mass function of H(%i; %i; %i)', wLang)
			main <- sprintf(fmt, hyper$N, hyper$M, hyper$n)
		}
		if (dist==3) {
			y    <- dpois(x, lambda = pois$lambda)
			fmt  <- getText('Probability mass function of Po(%.2f)', wLang)
			main <- sprintf(fmt, pois$lambda)
		}
	  pusr <- merge(c(-0.5, 1.7+1.2*max(x), 0, 1.2*max(y)), refit)
	  mp   <- barplot(y, xlim=pusr[1:2], ylim=pusr[3:4], axes=F, xaxs='i', yaxs='i',
	  								ylab="f(x)", xlab="x", main=main,
	  								cex.axis=cex, cex.lab=cex, cex.main=1.2*cex, cex.sub=cex)
	  axis(1, at=mp, labels=x, cex.axis=cex)
	  axis(2, cex.axis=cex)	
	} else {
		if (dist==1) {
			y    <- pbinom(x, size=binom$size, prob=binom$prob)
			fmt  <- getText('Cumulative distribution function of B(%i; %.2f)', wLang)
			main <- sprintf(fmt, binom$size, binom$prob)
		}
		if (dist==2) {
			y    <- phyper(x, m=hyper$M, n=hyper$N-hyper$M, k=hyper$n)
			fmt  <- getText('Cumulative distribution function of H(%i; %i; %i)', wLang)
			main <- sprintf(fmt, hyper$N, hyper$M, hyper$n)
		}
		if (dist==3) {
			y    <- ppois(x, lambda = pois$lambda)
			fmt  <- getText('Cumulative distribution function of Po(%.2f)', wLang)
			main <- sprintf(fmt, pois$lambda)
		}
	  pusr <- merge(c(-0.5, max(x)+0.5, -0.05, 1.05), refit)
	  plot(cdf(x, y), xlim=pusr[1:2], ylim=pusr[3:4], axes=F, xaxs='i', yaxs='i',
	  		 ylab="F(x)", xlab="x", main=main,
	  		 cex.axis=cex, cex.lab=cex, cex.main=1.2*cex, cex.sub=cex)
	  axis(1, at=x, labels=x, cex.axis=cex)
	  axis(2, cex.axis=cex)
	}
	graphics::box()
}

ui <- dashboardPage(
	dashboardHeader(title="MM*Stat", titleWidth=300),
	dashboardSidebar(width=300,
									 uiOutput("widgetDist"),
									 uiOutput("widgetParam"),
									 uiOutput("widgetPCDF"),
									 sidebarMenu(
									 	 menuItemOutput("options")
									 )
	),
	dashboardBody(
		fluidRow(
			box(plotOutput("out"))
		)
	)
)

server <- function(input, output, session) {
	
	widgetObserve(wHyper, input, session)
	output$widgetDist  <- renderUI({ renderWidget(wDist, 
																								session=session) })
	output$widgetParam <- renderUI({ ret <- renderPanel(getInputs(wDist),
	                                             wBinom,
	                                             wHyper,
	                                             wPois,
																							 session=session)
	ret
	})
	output$widgetPCDF <- renderUI({ renderWidget(wPCDF,
																							 session=session) })
	output$options <- renderUI({
		menuItem(getText('Options', wLang),
						 renderWidget(wLang, 
						 						 session=session),
						 renderWidget(wFont, 
						 						 session=session))
	})
	
		
	output$out <- renderPlot({
	  button <- widgetValueChanged(wPCDF, input)
	  makePlot(input[[getInputs(wDist)]],
	           getValues(wBinom, input),
	           getValues(wHyper, input),
	           getValues(wPois, input),
	           getValues(wPCDF, input),
	           button$refit,
	  				 input[[getInputs(wFont)]])
	})
}

shinyApp(ui, server)