library("shiny")
library("shinydashboard")
library("mmstat")

ui <- dashboardPage(
	dashboardHeader(title="Test", titleWidth=300),
	dashboardSidebar(width=300,
									 uiOutput("widgetUI"),
									 uiOutput("langUI")
									 
	),
	dashboardBody(
		fluidRow(
			box(verbatimTextOutput("out"), width="420px", height="320px")
		)
	)
)

l <- widgetLanguage('lang')
w <- widgetNormal('widget', lang=l)
 
server <- function(input, output, session) {

	output$widgetUI <- renderUI({ renderWidget(w, lang=input[[getInputs(l)]]) })
	output$langUI   <- renderUI({ renderWidget(l, lang=input[[getInputs(l)]]) })
	
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
