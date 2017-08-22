widgetSeed <- function(inputId,
											 button=list(),
											 seed=NULL,
											 lang=NULL) {
	env <- widget(inputId, lang)
	setUIelem (env, 'seed', 'actionButton', button, 
						 list(inputId='seed', label='Reset seed'))
	env[['data']] <- seed
	### observer
	env[['observe']] <- expression(observe({
		inp <- getInputs(env)
		N <- input[[inp[1]]]
		if (is.null(env[['data']])) set.seed (as.numeric(Sys.time())) else set.seed(as.numeric(env[['data']]))
	}))
	return(env)
}
