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

# three variables
l <- widgetLanguage('lang')
w <- widgetDataset('widget', lang=l) 
widgetVariables(w, list(), list(), list())

server <- function(input, output, session) {
  
  widgetObserve(w, input, session)
  output$widgetUI <- renderUI({ renderWidget(w, lang=input[[getInputs(l)]]) })
  output$langUI   <- renderUI({ renderWidget(l, lang=input[[getInputs(l)]]) })
  
  output$out <- renderPrint({
    inp <- getValues(w, input)
    printVar(inp)
    #
    printVar(getInputs(w))
    #
    dfs <- getVariables(w, input, session)
    printVar(dfs)
    #
    inp <- getValues(l, input)
    printVar(inp)
    #
    printVar(getInputs(l))
    #
  })
}

shinyApp(ui, server)
