library("shiny")
library("shinydashboard")
library("mmstat")
library("vcd")
library("DescTools")
library("lazyeval")
library("ryouready")

wLang  <- widgetLanguage('lang')
wFont  <- widgetFontSize('font', lang=wLang)
wTable <- widgetSelect(list(inputId='table',
													  label='Table view',
														choices=enumChoices(
															        c('Absolute frequencies',
																	    'Relative frequencies',
																			'Rowwise frequencies',
																			'Columnwise frequencies',
																			'Expected frequencies',
																			'Residuals',
																			'Studentized residuals',
														       		'Squared residuals'))
                                  ),
											        lang=wLang)
wCoeff <- widgetSelect(list(inputId='coeff',
														label='Coefficients',
														choices=enumChoices(
															        c('Chi square',
																			'Contingency',
																			'Corrected contingency',
																			'Cramers V/Phi',
																			'Goodman & Kruskals Lambda',
																			'Goodman & Kruskals Tau',
																			'Uncertainty')), 
														size=3,
														selectize = FALSE,
														multiple  = TRUE),
												lang=wLang)
wData <- widgetDataset('widget', lang=wLang)
widgetVariables(wData, list(vartype=is.factor), list(vartype=is.factor))

mmstat <- new.env()

htmlTable <- function(tab, fmt, left=NULL, top=NULL, right=NULL, bottom=NULL, rowlab=NULL, collab=NULL) {
	ncol <- ncol(tab)+if(is.null(left)) 0 else 1+if(is.null(right)) 0 else 1
  html <- '<table width="95%" margin="8px">'
  if (!is.null(collab)) html <- paste0(html, sprintf('<tr><td colspan="%i" style="text-align:right;">%s</td></tr>', ncol, collab))
  if (!is.null(top)) {
  	row <-'' 
  	if(!is.null(left)) row <- paste0(row, sprintf('<td></td>'))
  	for (i in seq(length(top))) {
      row <- paste0(row, sprintf('<td style="text-align:right;background-color:#BBBBBB"><b>%s</b></td>', top[i]))  		
  	}
  	if(!is.null(right)) row <- paste0(row, '<td></td>')
  	html <- paste0(html, '<tr>', row, '</tr>')
  }
  for (i in seq(nrow(tab))) {
  	row <- ''
  	if(!is.null(left)) row <- paste0(row, sprintf('<td style="background-color:#BBBBBB"><b>%s</b></td>', left[i]))
  	for (j in seq(ncol(tab))) {
  		row <- paste0(row, 
  									sprintf('<td style="text-align:right;background-color:%s">', if(i%%2) '#FFFFFF' else '#DDDDDD'),
  									sprintf(fmt, tab[i,j]), 
  									'</td>')
  	}
  	if(!is.null(right)) row <- paste0(row, '<td style="text-align:right;text-align:right;background-color:#BBBBBB">', sprintf(fmt, right[i]), '</td>')
  	html <- paste0(html, '<tr>', row, '</tr>')
  }
	if (!is.null(bottom)) {
		row <-'' 
		if(!is.null(left)) row <- paste0(row, sprintf('<td></td>'))
		for (i in seq(length(bottom))) {
			row <- paste0(row, sprintf('<td style="text-align:right;background-color:#BBBBBB">%s</td>',  sprintf(fmt, bottom[i])))  		
		}
		if(!is.null(right)) row <- paste0(row, '<td></td>')
		html <- paste0(html, '<tr>', row, '</tr>')
	}
  if (!is.null(rowlab)) html <- paste0(html, sprintf('<tr><td colspan="%i" align="left">%s</td></tr>', ncol, rowlab))
  html <- paste0(html, '</table>')
  return(html)
}

makeTable <- function(row, col, rowlab="y", collab="x",	coeff=0, view=1, fonsize=1) {
	if (anyUndefined(row, col, view, fonsize)) return(NULL)
	tab    <- table(row, col) 
	top    <- levels(col)
	left   <- levels(row)
	bottom <- right <- NULL
  if (!(view%in%1:7)) view <- 1 
	if (view==1) {
		fmt    <- "%i"
		right  <- rowSums(tab)
		bottom <- colSums(tab)
	}
	if (view==2) {
		 tab <- prop.table(tab); 
		 fmt <- "%.3f"
		 right  <- rowSums(tab)
		 bottom <- colSums(tab)
	}
	if (view==3) {
		bottom <- colSums(prop.table(tab))
		tab <- prop.table(tab, 1); 
		fmt <- "%.3f"
		right  <- rowSums(tab)
		
	}
	if (view==4) {
		right <- rowSums(prop.table(tab))
		tab <- prop.table(tab, 2); 
		fmt <- "%.3f"
		bottom <- colSums(tab)	#env                     <- widget(title, lang)

	}
	if (view==5) {
		csum <- colSums(tab) 
	  rsum <- rowSums(tab) 
		tab  <- outer(rsum, csum)/sum(tab)
		fmt  <- "%.1f"
		right  <- rowSums(tab)
		bottom <- colSums(tab)
	}
	if (view==6) {
		csum <- colSums(tab) 
		rsum <- rowSums(tab) 
		expe <- outer(rsum, csum)/sum(tab)
		tab  <- (tab-expe)/sqrt(expe) 
		fmt  <- "%.2f"
	}
	if (view==7) {
		csum <- colSums(tab) 
		rsum <- rowSums(tab) 
		expe <- outer(rsum, csum)/sum(tab)
		tab  <- scale((tab-expe)^2/expe) 
		fmt  <- "%.2f"
	}
	if (view==8) {
		csum <- colSums(tab) 
		rsum <- rowSums(tab) 
		expe <- outer(rsum, csum)/sum(tab)
		tab  <- (tab-expe)^2/expe 
		fmt  <- "%.2f"
	}
	
	tf   <- (1:7 %in% coeff)
	ctab <- ''
	if (any(tf)) {
		basetab <- table(row, col)
		crmin   <- min(ncol(basetab), nrow(basetab))
		astat   <- assocstats(basetab)
		cval    <- numeric(0)
		cname   <- character(0)

		if (tf[1]) {
			cval  <- c(cval, astat$chisq_tests[2,1])	
			cname <- c(cname, "Chi square")
		}
		if (tf[2]) {
			cval  <- c(cval, astat$contingency)	
			cname <- c(cname, "Contingency")
		}
		if (tf[3]) {
			cval  <- c(cval, astat$contingency*sqrt(crmin/(crmin-1)))	
			cname <- c(cname, "Corrected contingency")
		}
		if (tf[4]) {
			cval  <- c(cval, astat$cramer)	
			cname <- c(cname, "Cramers V/Phi")
		}
		if (tf[5]) {
			cval  <- c(cval, val$lambda.cr)	
			cname <- c(cname, sprintf("Goodman & Kruskal's lambda (%s dependend)", collab))
			cval  <- c(cval, val$lambda.rc)	
			cname <- c(cname, sprintf("Goodman & Kruskal's lambda (%s dependend)", rowlab))
			cval  <- c(cval, val$lambda.symmetric)	
			cname <- c(cname, "Goodman & Kruskal's lambda (symmetric)")
		}
		if (tf[6]) {
			cval  <- c(cval, GoodmanKruskalTau(basetab))	
			cname <- c(cname, sprintf("Goodman & Kruskal's tau (%s dependend)", collab))
			cval  <- c(cval, GoodmanKruskalTau(basetab, dir='c'))	
			cname <- c(cname, sprintf("Goodman & Kruskal's tau (%s dependend)", rowlab))
		}
		if (tf[7]) {
			val   <- nom.uncertainty(basetab)
			cval  <- c(cval, val$uc.cr)	
			cname <- c(cname, sprintf("Uncertainty (%s dependend)", collab))
			cval  <- c(cval, val$uc.rc)	
			cname <- c(cname, sprintf("Uncertainty  (%s dependend)", rowlab))
			cval  <- c(cval, val$uc.symmetric)	
			cname <- c(cname, "Uncertainty (symmetric)")
		}
		ctab <- htmlTable(as.table(as.matrix(cval, ncol=1)), fmt="%.3f", left=cname, top="Value")
	}
	paste0(htmlTable(tab, fmt, right=right, bottom=bottom, top=top, left=left, collab=collab, rowlab=rowlab),
				 ctab)
}

ui <- dashboardPage(
	dashboardHeader(title="MM*Stat", titleWidth=300),
	dashboardSidebar(width=300,
									 uiOutput("widgetTable"),
									 uiOutput("widgetCoeff"),
									 uiOutput("widgetData"),
									 sidebarMenu(
									 	 menuItemOutput("options")
									 )
	),
	dashboardBody(
		fluidRow(
      htmlOutput("out")
		)
	)
)

# panelTabBox(..., id = "tabset", intro=NULL, selected = NULL, title = NULL, width = 12, height = NULL, side = c("left", "right"), lang=NULL)
# panelMain(title="Main", ..., value = title, icon = NULL, lang=NULL)
# panelHelp(title="Data",  value = title, icon = NULL, lang=NULL)
# panelData(title="Help",  value = title, icon = NULL, lang=NULL)
#
#

server <- function(input, output, session) {
	
	widgetObserve(wData, input, session)
	output$widgetTable <- renderUI({ renderWidget(wTable, 
#																								lang=input[[getInputs(wLang)]],
																								session=session) })
	output$widgetCoeff <- renderUI({ renderWidget(wCoeff, 
#																								lang=input[[getInputs(wLang)]],
																								session=session) })
	output$widgetData <- renderUI({ renderWidget(wData, 
#																							lang=input[[getInputs(wLang)]],
																							session=session) })
	#
	output$options <- renderUI({
		menuItem(getText('Options', wLang),
						 renderWidget(wLang, 
#						 						 lang=input[[getInputs(wLang)]],
						 						 session=session),
						 renderWidget(wFont, 
#						 						 lang=input[[getInputs(wLang)]],
						 						 session=session))
	})
	
	output$out <- renderUI({
#		browser()
		data  <- getValues(wData, input)
		if (anyUndefined(data$dataset, data$var1, data$var2)) return('')
		view  <- getValues(wTable, input)
		coeff <- getValues(wCoeff, input)
		#
		vars <- getVariables(wData, input, session)
		tab  <- table(vars[[1]][,1], vars[[2]][,1], dnn=c(names(vars[[1]])[1], names(vars[[2]])[1]))
		ct   <- contingencyTable(tab)
#			'Absolute frequencies',
#							'Relative frequencies',
#							'Rowwise frequencies',
#							'Columnwise frequencies',
#							'Expected frequencies',
#							'Residuals',
#							'Studentized residuals',
#							'Squared residuals')
		cth <- switch(as.numeric(view$table),
									table2html(ct$observed, colsums="Column sums", rowsums="Row sums"),
									table2html(ct$observed/sum(ct$observed), digits=3, colsums="Column sums", rowsums="Row sums"),
									table2html(ct$observed/rowSums(ct$observed), digits=3, rowsums="Row sums"),
									table2html(ct$observed/colSums(ct$observed), digits=3, colsums="Column sums"),
									table2html(ct$expected, digits=3, colsum="Column sums", rowsums="Row sums"),
									table2html(ct$residuals, digits=3),
									table2html(ct$stdres, digits=3),
									table2html(ct$residuals^2, digits=3, colsum="Column sums", rowsums="Row sums")
					 )
		ctab <- switch(as.numeric(coeff$))
	  HTML(cth)
	})
}

shinyApp(ui, server)