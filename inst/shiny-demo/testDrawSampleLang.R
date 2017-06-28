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

counter <- 0
l <- widgetLanguage('lang')
w <- widgetDrawSample('widget', lang=l)

server <- function(input, output, session) {

	output$widgetUI <- renderUI({ renderWidget(w, lang=input[[getInputs(l)]]) })
	output$langUI   <- renderUI({ renderWidget(l, lang=input[[getInputs(l)]]) })
	
	widgetObserve(l, input, session)
	widgetObserve(w, input, session)
	
  output$out <- renderPrint({
  	inp <- getValues(w, input)
  	printVar(inp)
  	# show Inputs
    printVar(getInputs(w))
  	# check if button pressed
  	val <- widgetValueChanged(w, input)
  	printVar(val)
  	# 
  	widgetInvalidate(inp$speed)
    # or: widgetInvalidate(input$widget.speed)
    counter <<- counter+1
  	printVar(counter)
   })
}

shinyApp(ui, server)
