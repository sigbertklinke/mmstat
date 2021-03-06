library("shiny")
library("shinydashboard")
library("mmstat")

ui <- dashboardPage(
	dashboardHeader(title="Test", titleWidth=300),
	dashboardSidebar(width=300,
									 uiOutput("widgetUI"),
									 uiOutput("langUI"),
									 uiOutput("helpUI")
									 
	),
	dashboardBody(
		fluidRow(
			box(verbatimTextOutput("out"), width="420px", height="320px")
		)
	)
)

l <- widgetLanguage('lang')
w <- widgetBinomial('widget', lang=l)
h <- widgetHelpButton('help', url='https://en.wikipedia.org/wiki/Binomial_distribution', lang=l)

server <- function(input, output, session) {
  widgetObserve(l, input, session)
  widgetObserve(w, input, session)  
  widgetObserve(h, input, session)
  
  output$helpUI <- renderUI({ renderWidget(h) })
  output$widgetUI <- renderUI({ renderWidget(w) })
  output$langUI   <- renderUI({ renderWidget(l) })
	
  output$out <- renderPrint({
  	inp <- getValues(w, input)
  	printVar(inp)
  	#
  	printVar(getInputs(w))
  	#
  	inp <- getValues(l, input)
  	printVar(inp)
  	#
  	printVar(getInputs(l))
  	#
   })
}

shinyApp(ui, server)
