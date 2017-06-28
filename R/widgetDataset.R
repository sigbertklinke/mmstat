#' Widget for data set and variable selection
#'
#' Reads in all \code{*.rds} or \code{*.RDS} files from the first non-empty directory in \code{pathes}
#' and extracts the variables. Each file must contain one data frame only.
#' The widget must be used together with \code{widgetVariables}.
#
#' @param inputId character: widget name
#' @param pattern character: a \link{regular expression}. Only file names which match the regular
#' expression will be read (default: \code{c('^[[:alnum:]]+.rds$', '^[[:alnum:]]+.RDS$')})
#' @param pathes character: directories to read data files from (default: \code{c('.', find.package('mmstat'))})
#' @param duplicates.ok logical: can variables be in a selection be used more than once? (default: FALSE)
#' @param datasettype logical function: should data set used? (default: \code{is.data.frame})
#' @param data list: \code{selectInput} parameter for the data set(s) (default: \code{list()})
#' @param lang widget: language widget (or NULL)
#'
#' @seealso \code{\link{selectInput}}, \code{\link{sliderInput}}, \code{\link{widgetVariables}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   demo(testDataset1) # one variable
#'   demo(testDataset2) # two variables
#'   demo(testDataset3) # two variables: one numeric, one factor
#'   demo(testDataset3Lang)
#' }
widgetDataset <- function (inputId,
                           pattern = c('^[[:alnum:]]+.rds$', '^[[:alnum:]]+.RDS$'),
													 pathes  = c('.', find.package('mmstat')),
                           duplicates.ok = FALSE,
													 datasettype = is.data.frame,  # default: accept data frames only
                           data = list(),
                           lang = NULL) {
  init <- function(pattern, datasettype) {
    data     <- list()
    dataname <- c()
    datai    <- 1
    dscheck  <- match.fun(datasettype)
#    path     <- getwd()
#    setwd (paste0(path, '/../inst'))
    for (path in pathes) {
      for (pat in pattern) {
        files <- list.files(path=path, pattern=pat, full.names=T)
        names <- sapply(strsplit(basename(files), '.', fixed=T), function(elem) { elem[1] })
        for (i in seq(files)) {
      	  fconti <- readRDS(files[i])
      	  if (dscheck(fconti)) {
            data[[datai]]   <- fconti   
            dataname[datai] <- names[i]
            datai <- datai+1
      	  }
        }
      }
    	if (length(data)>0) break
    }
    names(data) <- dataname
#    setwd(path)
    return(data)
  }

  env                    <- widget(inputId, lang)
  # init dataset
  env[['data']]          <- init(pattern, datasettype)
  choices                <- enumChoices(names(env[['data']]))
  env[['duplicates.ok']] <- duplicates.ok
  # init vars
#  sel     <- vector("list", length(env[['data']]))
#  invalid <- rep(F, length(env[['data']])) 
#  for (i in 1:length(sel)) {
#  	sel[[i]] <- matrix(1, nrow=(length(env[['data']][[i]])), ncol=length(vars))
#  	for (j in seq(length(vars))) {
#  		if (!is.null(vars[[j]]$vartype)) {
#  			sel[,j] <- as.numeric(sapply(env[['data']][[i]], vars[[j]]$vartype))
#  			vars[[i]]$vartype <- NULL
#  		}
#  	} 	
#  	if (duplicates.ok) { 
#  		invalid[i] <- any(colSums(sel[[i]])==0)
#  	} else {
#  		invalid[i] <- sum(apply(sel[[i]], 1, any)>=length(vars))
#  	}
#  }
#  choices[[invalid]] <- NULL
#  stopif(length(choices)==0, 'No data set available with the defined variable(s) restrictions')
#  env[['selection']] <- sel
#  # Dataset selector
  args              <- list(inputId='dataset', label="Choose data set",
                            choices=choices)
  env[['ui']]$data  <- list(func='selectInput',
                            args=mergeListsByName(args, data))
#  data <- as.numeric(choices[[1]])
#  for (j in seq(length(vars))) {
#    vari <- sprintf('var%i', i)
#    args <- list(inputId = vari,
#                 label   = sprintf('Variable %i', i), 
#                 choices = enumChoices(names(env[['data']][[data]])[sel[data,]==1]))
#    env[['ui']][[vari]] <- list(func='selectInput',
#                                args=mergeListsByName(args, vars[[i]]))
#  }  
  return(env)
}

#' Widget to select variables of a dataset
#'
#' This widget needs an \code{\link{widgetObserve}} to update data set changes to the variable selectors.
#'
#' Usually all variables of data frame are offered. If the parameter list for \code{selectInput}s
#' contain a parameter \code{vartype=func} then only the variables can be selected for which the
#' \code{func} delivers \code{TRUE}. An example would be \code{vartype=is.factor} such that the
#' variable selector just offers the categorical variables of the data frame.
#' The widget must be used together with \code{widgetDataset}.
#'
#' @note The function will produce a Shiny error if not all data sets read contain at least one
#' appropiate variable.
#'
#' @param env widget: data set widget
#' @param ... lists: parameter lists for \code{selectInput} to select one variable
#'
#' @seealso \code{\link{selectInput}}, \code{\link{sliderInput}}, \code{\link{widgetDataset}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#'   demo(testDataset1) # one variable
#'   demo(testDataset2) # two variables
#'   demo(testDataset3) # two variables: one numeric, one factor
#'   demo(testDataset3Lang)
#' }
widgetVariables <- function (env, ...) {
	# Variable selectors
	vars <- list(...)
	if (length(vars)==0) vars <- list(list())
	#
	sel     <- vector("list", length(env[['data']]))
  invalid <- rep(F, length(env[['data']])) 
  for (i in 1:length(sel)) {
  	sel[[i]] <- matrix(1, nrow=(length(env[['data']][[i]])), ncol=length(vars))
  	for (j in seq(length(vars))) {
  		if (!is.null(vars[[j]]$vartype)) {
  			sel[[i]][,j] <- as.numeric(sapply(env[['data']][[i]], vars[[j]]$vartype))
  		}
  	} 	
  	# can for each selectInput at least one variable selected?
  	invalid[i] <- any(colSums(sel[[i]])==0) 
  	if (!env[['duplicates.ok']]) {
  		# do we have less selectable variables than selectInputs? 
  		# we may need here a finer condition?
 		  invalid[i] <- invalid[i] | sum(apply(sel[[i]]==1, 1, any))<length(vars)
  	}
  }
  #browser()
  if (any(invalid)) env[['ui']]$data$args$choices[invalid] <- NULL 
  stopif(length(env[['ui']]$data$args$choices)==0, 
  			 'No data set available with the defined variable(s) restrictions')
  env[['selection']] <- sel
  #browser()
  # Variable selectors
  for (j in seq(length(vars))) {
  	vars[[j]]$vartype    <- NULL
  	varj                 <- sprintf("var%i", j)
  	args                 <- list(inputId=varj, label="Choose variable", choices=NULL)
  	env[['ui']][[varj]]  <- list(func='selectInput',
  															 args=mergeListsByName(args, vars[[j]]))
  }	
}

getSelection <- function (seld, duplicates.ok, vars) {
	# 0 variable not selectable
	# 1 variable selectable
	# 2 variable is selected 
	#browser()
	for (i in 1:length(vars)) {
		for (j in 1:length(vars[[i]])) {
			if (length(vars[[i]])>0) { # anything selected?
  			vj <- vars[[i]][j]
	  		if (!is.na(vj) && (vj<=nrow(seld)) && (seld[vj,i]==1)) {
		  		if (!duplicates.ok) seld[vj,] <- sign(seld[vj,])
			  	seld[vj,i] <- 2
	  		}
			}
		}
	}
	for (i in 1:ncol(seld)) {
	  if (sum(seld[,i]==2)==0) { # nothing selected
	    pos <- which(seld[,i]>0)
	    stopif (length(pos)==0, "Can not select a variable") # ???
	  	if (duplicates.ok) {
	    	seld[i, pos[1]] <- 2
	    } else {
	    	for (j in 1:length(pos)) {
	    		if (all(seld[pos[j],]<2)) {
	    			seld[pos[j], i] <- 2
	    			break
	    		}
	    	}
	    }	
	    pos <- which(seld[,i]==2)
	    stopif (length(pos)==0, "Can not select a variable") # ???
	  }
	}
	ret <- vector("list", ncol(seld))
	for(i in 1:ncol(seld)) {
		ret[[i]] <- list(selected=which(seld[,i]==2), choices=which(seld[,i]>0))
		if (!duplicates.ok) {
			for(j in 1:ncol(seld)) {
				if (j!=i) ret[[i]]$choices <- setdiff(ret[[i]]$choices, which(seld[,j]==2))
			}
		}
	}
	ret
}


#' Returns variable(s) of a data set
#'
#' @param env data set widget
#' @param input input object: current values of all widgets in your app. Is the first parameter of the \code{server} function.
#' @param session session object: is the third optional parameter of the \code{server} function.
#'
#' @seealso \code{\link{widgetDataset}}
#'
#' @return a list of data frames, one for each var element in \code{widgetDataset}
#' @export
getVariables <- function(env, input, session) {
  #browser()
  inpv <- sapply(env[["ui"]], function(l) {
    l$args$inputId
  })
  inp  <- getInputs(env)
	data <- as.numeric(input[[inp[1]]]) # data set index
	if (length(data)==0) {
		data <- as.numeric(env[['ui']]$data$args$choices[[1]])
		updateSelectInput(session, inp[1], selected = data)
	}
	nvar <- length(inp)-1
	dfs  <- list()
	vars <- vector("list", nvar)
	for (i in 1:nvar) {
		dfs[[inpv[i+1]]]  <- env[['data']][[data]][,FALSE] # create empty data frames
		vars[[i]] <- as.numeric(input[[inp[i+1]]])
	}
	ret <- getSelection(env[['selection']][[data]], env[['duplicates.ok']], vars)
	#browser()
	varnames <- names(env[['data']][[data]])
  for (i in 1:nvar) {
  	selvar   <- rep(F, length(varnames))
  	selvar[ret[[i]]$choices] <- T
    choices  <- enumChoices(varnames, select=selvar)
  	updateSelectInput(session, inp[i+1], selected = ret[[i]]$selected, choices=choices)
  	dfs[[inpv[i+1]]] <- as.data.frame(env[['data']][[data]][,ret[[i]]$selected])
  	names(dfs[[inpv[i+1]]]) <- names(env[['data']][[data]])[ret[[i]]$selected]
  }
  return(dfs)
}
