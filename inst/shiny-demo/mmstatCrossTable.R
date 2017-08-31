library("shiny")
library("shinyBS")
library("shinydashboard")
library("mmstat")
library("DescTools")

wLang  <- widgetLanguage('lang')
wFont  <- widgetFontSize('font', lang=wLang)
wTable <- widgetSelect(list(inputId='table',
													  label='Table view',
														choices=enumChoices(
															        'Absolute frequencies',
																	    'Relative frequencies',
																			'Rowwise frequencies',
																			'Columnwise frequencies',
																			'Expected frequencies',
																			'Residuals',
																			'Studentized residuals',
														       		'Squared residuals')
                                       ),
											 lang=wLang)
wCoeff <- widgetSelect(list(inputId='coeff',
														label='Coefficients',
														choices=enumChoices(
															        'Chi square based',
																			'Goodman & Kruskals Lambda',
																			'Goodman & Kruskals Tau',
																			'Uncertainty'), 
														size=3,
														selectize = FALSE,
														multiple  = TRUE),
											 tooltip = "Use Ctrl/Shift to select more than one entry",
											 lang=wLang)
wData <- widgetDataset('widget', lang=wLang)
widgetVariables(wData, list(vartype=is.factor), list(vartype=is.factor))

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

server <- function(input, output, session) {
	
	widgetObserve(wLang, input, session)
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
		view  <- getValues(wTable, input, TRUE)
		coeff <- getValues(wCoeff, input, TRUE)
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
		cth <- switch(as.numeric(view),
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
		ctab <- poText(association(ct, chisquare=(1 %in% coeff), lambda=(2 %in% coeff), gkt=(3 %in% coeff),
		    							  			     theil=(4 %in% coeff)),
									 wLang)
		if (!is.null(ctab)) cth<-paste0(cth, table2html(ctab, digits=3)) 
		HTML(cth)
	})
}

shinyApp(ui, server)