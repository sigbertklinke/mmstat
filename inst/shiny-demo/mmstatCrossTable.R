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
#		c('Chi square',
#			'Contingency',
#			'Corrected contingency',
#			'Cramers V/Phi',
#			'Goodman & Kruskals Lambda',
#			'Goodman & Kruskals Tau',
#			'Uncertainty')), 
		ctab <- poText(association(ct, chisq=(1 %in% coeff$coeff), C=(2 %in% coeff$coeff),
									  			     CC=(3 %in% coeff$coeff), V=(4 %in% coeff$coeff),
    								   				 lambda=(5 %in% coeff$coeff), gkt=(6 %in% coeff$coeff),
		    							  			 theil=(7 %in% coeff$coeff)),
									 wLang)
		if (!is.null(ctab)) cth<-paste0(cth, table2html(ctab, digits=3)) 
		HTML(cth)
	})
}

shinyApp(ui, server)