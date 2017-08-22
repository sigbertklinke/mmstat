library("shiny")
library("shinydashboard")
library("mmstat")

wLang  <- widgetLanguage('lang')
wFont  <- widgetFontSize('font', lang=wLang)
wLevel <- widgetConfidenceLevel('wLevel',
	                              lang=wLang)
wSamples <- widgetSlider(list(inputId='samples',
															min=1, max=100, value=25,
															label="Select number of samples"),
												 lang=wLang)
wDraw   <- widgetActionButton(list(inputId='newsample',
															     label="Draw new samples"),
													    lang=wLang)
wSize   <- widgetSlider(list(inputId='size',
														 min=30, max=500, value=30, step=10,
														 label="Select sample size"),
												lang=wLang)
wVar    <- widgetCheckbox(list(inputId='var',
														   label="Population variance known"),
												  lang=wLang)

mmstat <- new.env()
ui <- dashboardPage(
	dashboardHeader(title="MM*Stat", titleWidth=300),
	dashboardSidebar(width=300,
									 uiOutput("widgetLevel"),
									 uiOutput("widgetSamples"),
									 uiOutput("widgetSampleSize"),
									 uiOutput("widgetDraw"),
									 uiOutput("widgetVar"),
									 sidebarMenu(
									 	 menuItemOutput("options")
									 )
	),
	dashboardBody(
		fluidRow(
			 plotOutput("out")
		)
	)
)

x <- runif(500)


server <- function(input, output, session) {
	
	widgetObserve(wLang, input, session)
	output$widgetLevel      <- renderUI({ renderWidget(wLevel, session=session) })
	output$widgetSamples    <- renderUI({ renderWidget(wSamples, session=session) })
	output$widgetSampleSize <- renderUI({ renderWidget(wSize, session=session) })
	output$widgetDraw       <- renderUI({ renderWidget(wDraw, session=session) })
	output$widgetVar        <- renderUI({ renderWidget(wVar, session=session) })

	observeEvent()
	
	output$options <- renderUI({
		menuItem(getText('Options', wLang),
						 renderWidget(wLang, 
						 						  session=session),
						 renderWidget(wFont, 
						 						  session=session))
	})
	
	output$out <- renderPlot({
		sd <- NULL
		if (input[[getInputs(wVar)]]) sd <- sd(x)
    ci <- confidenceInterval(x, param=mean(x), CI=CI.mean, sample=input[[getInputs(wSamples)]],
    												 size=input[[getInputs(wSize)]], level=input[[getInputs(wLevel)]]/100,
    												 sd=sd)
    plot(ci, main=sprintf("%.1f%% confidence interval for the mean", input[[getInputs(wLevel)]]))
	})
}

shinyApp(ui, server)