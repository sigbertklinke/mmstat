library("shiny")
library("shinydashboard")
library("mmstat")

wLang  <- widgetLanguage('lang')
wFont  <- widgetFontSize('font', lang=wLang)
wDist  <- widgetSelect(list(inputId='dist',
													  label="Select a distribution type",
														choices=c("Exponential distribution",
																			"Normal distribution")),
											 lang=wLang)
wExp   <- widgetIntensity('exp', lang=wLang)
wNorm  <- widgetNormal('norm', lang=wLang)
wPCDF  <- widgetPCDF('pcdf', pcdf=list(choices=c("Probability density function", 
                                                 "Cumulative distribution function")), 
                     lang=wLang)

mmstat <- new.env()

makePlot <- function(dist, exp, norm, pcdf, refit, cex) {
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
	#
#	str(dist)
#	str(binom)
#	str(pois)
#	str(hyper)
#	str(pcdf)
#	str(refit)
#	str(cex)
	if (anyUndefined(dist, norm, exp, pcdf, refit, cex)) return(NULL)
	#
	xr <- switch(dist, 
	             '1'=c(0, qexp(0.999, exp$lambda)),
		 					 '2'=norm$mean+norm$sd*c(qnorm(0.001), qnorm(0.999)))
  x  <- min(xr)+(0:500)/500*diff(range(xr))			
	if (pcdf$pcfunc==1) { 
		if (dist==1) {
		  y    <- dexp(x, rate=exp$lambda)
		  fmt  <- getText('Probability density function of Exp(%.2f)', wLang)
		  main <- sprintf(fmt, exp$lambda)
		}
		if (dist==2) {
			y    <- dnorm(x, mean=norm$mean, sd=norm$sd)
			fmt  <- getText('Probability density function of N(%.2f; %.2f)', wLang)
			main <- sprintf(fmt, norm$mean, norm$sd)
		}
	  pusr <- merge(c(min(x), max(x), 0, 1.2*max(y)), refit)
	  plot(x, y, type="l", xlim=pusr[1:2], ylim=pusr[3:4], xaxs='i', yaxs='i',
	       ylab="f(x)", xlab="x", main=main,
	       cex.axis=cex, cex.lab=cex, cex.main=1.2*cex, cex.sub=cex)
	} else {
	  if (dist==1) {
	    y    <- pexp(x, rate=exp$lambda)
	    fmt  <- getText('Cumulative distribution function of Exp(%.2f)', wLang)
	    main <- sprintf(fmt, exp$lambda)
	  }
	  if (dist==2) {
	    y    <- pnorm(x, mean=norm$mean, sd=norm$sd)
	    fmt  <- getText('Cumulative distribution function of N(%.2f; %.2f)', wLang)
	    main <- sprintf(fmt, norm$mean, norm$sd)
	  }
	  pusr <- merge(c(min(x), max(x), -0.1, 1.1*max(y)), refit)
	  plot(x, y, type="l", xlim=pusr[1:2], ylim=pusr[3:4], xaxs='i', yaxs='i',
	       ylab="F(x)", xlab="x", main=main,
	       cex.axis=cex, cex.lab=cex, cex.main=1.2*cex, cex.sub=cex)
	  abline(h = c(0, 1), lty = 2)
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
			tabBox(
				title = "First tabBox",
				id = "tabset",
				width = 12,
				tabPanel("Main", plotOutput("out")),
				tabPanel("Help", "Help")
			)
		)
	)
)



server <- function(input, output, session) {
	
	output$widgetDist  <- renderUI({ renderWidget(wDist, 
																								lang=input[[getInputs(wLang)]],
																								session=session) })
	output$widgetParam <- renderUI({ ret <- renderPanel(getInputs(wDist),
	                                             wExp,
	                                             wNorm,
																							 lang=input[[getInputs(wLang)]],
																							 session=session)
	                                 ret
	})
	output$widgetPCDF <- renderUI({ renderWidget(wPCDF,
																							 lang=input[[getInputs(wLang)]],
																							 session=session) })
	output$options <- renderUI({
		menuItem(getText('Options', wLang),
						 renderWidget(wLang, 
						 						 lang=input[[getInputs(wLang)]],
						 						 session=session),
						 renderWidget(wFont, 
						 						 lang=input[[getInputs(wLang)]],
						 						 session=session))
	})
	
		
	output$out <- renderPlot({
	  button <- widgetValueChanged(wPCDF, input)
	  makePlot(input[[getInputs(wDist)]],
	           getValues(wExp, input),
	           getValues(wNorm, input),
	           getValues(wPCDF, input),
	           button$refit,
	  				 input[[getInputs(wFont)]])
	})
}

shinyApp(ui, server)