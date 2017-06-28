library("shiny")
library("shinydashboard")
library("mmstat")

ui <- dashboardPage(
	dashboardHeader(title="Test", titleWidth=300),
	dashboardSidebar(width=300,
									 uiOutput("widgetUI")
	),
	dashboardBody(
		fluidRow(
			box(verbatimTextOutput("out"), width="420px", height="320px")
		)
	)
)

counter <- 0
w <- widgetDrawSample('widget')

server <- function(input, output, session) {
  
	output$widgetUI     <- renderUI({ renderWidget(w) })
  
  output$out <- renderPrint({
  	inp <- getValues(w, input)
  	printVar(inp)
  	#
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
