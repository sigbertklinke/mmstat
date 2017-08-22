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
w <- widgetSlider(list(inputId='myslider', label='My slider', min=0, max=1, value=0.5, step=0.01),
									lang=l) 

server <- function(input, output, session) {
  
  widgetObserve(w, input, session)
  widgetObserve(l, input, session)
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
