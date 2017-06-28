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

w <- widgetBinomial('widget', size=list(value="session"))

server <- function(input, output, session) {
	
	output$widgetUI     <- renderUI({ renderWidget(w, session=session) })
	
	output$out <- renderPrint({
		inp <- getValues(w, input)
		printVar(inp)
		#
		printVar(getInputs(w))
	})
}

shinyApp(ui, server)
