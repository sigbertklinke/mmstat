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

w <- widgetPCDF('widget')

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
	})
}

shinyApp(ui, server)
