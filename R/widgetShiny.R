#' Encapsulates a Shiny input element 
#' 
#' This the basic function to encapsulate all shiny input elements. If you do not need a language change on-the-fly then you should use shiny input element.
#' traceback()
#' @param input input element: either a function or function name
#' @param param list: parameters for the input element
#' @param lang  language widget (or NULL)
#'
#' @return a widget object (environment)
#' @export
#' @import shiny
#'
#' @examples
#' library('shiny')
#' widgetShiny(sliderInput, list(inputId='slider', label='My slider', min=0, max=10), lang=NULL)
widgetShiny <- function (input, param, lang) {
	env               <- widget('shiny', lang)
	func              <- if (is.function(input)) as.character(substitute(input)) else as.character(input) 
	env[['ui']]$shiny <- list(func=func, args=param)
	return(env)
}

#' Encapsulates a Shiny \code{actionButton}
#'
#' Encapsulates a Shiny \code{actionButton}. If you do not need a language change on-the-fly then you should use \code{actionButton}.
#'  
#' @param param list of parameters for \code{actionButton}
#' @param lang  language widget (or NULL)
#' 
#' @seealso \code{\link{actionButton}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetActionButton(list(inputId="goButton", label='Go!'), lang=NULL)
widgetActionButton  <- function (param, lang) { widgetShiny(actionButton, param, lang) }

#' Encapsulates a Shiny \code{checkboxGroupInput}
#' 
#' Encapsulates a Shiny \code{checkboxGroupInput}. If you do not need a language change on-the-fly then you should use \code{checkboxGroupInput}.
#'
#' @param param list of parameters for \code{checkboxGroupInput}
#' @param lang  language widget (or NULL)
#' 
#' @seealso \code{\link{checkboxGroupInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetCheckboxGroup(list(inputId="variable", label='Variable', 
#'                          choices=list("Cylinders" = "cyl", 
#'                                       "Transmission" = "am", 
#'                                       "Gears" = "gear")), 
#'                     lang=NULL)
widgetCheckboxGroup <- function (param, lang) { widgetShiny(checkboxGroupInput, param, lang) }

#' Encapsulates a Shiny checkboxInput
#' 
#' Encapsulates a Shiny checkboxInput. If you do not need a language change on-the-fly then you should use checkboxInput.
#' 
#' @param param list of parameters for checkboxInput
#' @param lang  language widget (or NULL)
#'
#' @seealso \code{\link{checkboxInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetCheckbox(list(inputId="outliers", label='Show outliers'), lang=NULL)
widgetCheckbox      <- function (param, lang) { widgetShiny(checkboxInput, param, lang) }

#' Encapsulates a Shiny \code{dateInput}
#' 
#' Encapsulates a Shiny \code{dateInput}. If you do not need a language change on-the-fly then you should use \code{dateInput}.
#' 
#' @param param list of parameters for \code{dateInput}
#' @param lang  language widget (or NULL)
#'
#' @seealso \code{\link{dateInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetDate(list(inputId='date', label='Date:', value = '2012-02-29'), lang=NULL)
widgetDate          <- function (param, lang) { widgetShiny(dateInput, param, lang) }

#' Encapsulates a Shiny \code{fileInput}
#' 
#' Encapsulates a Shiny \code{fileInput}. If you do not need a language change on-the-fly then you should use \code{fileInput}.
#' 
#' @param param list of parameters for \code{fileInput}
#' @param lang  language widget (or NULL)
#' 
#' @seealso \code{\link{fileInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetFile(list(inputId='file', label='File:'), lang=NULL)
widgetFile          <- function (param, lang) { widgetShiny(fileInput, param, lang) }

#' Encapsulates a Shiny \code{numericInput}
#' 
#' Encapsulates a Shiny \code{numericInput}t. If you do not need a language change on-the-fly then you should use \code{numericInput}.
#' 
#' @param param list of parameters for \code{numericInput}
#' @param lang  language widget (or NULL)
#' 
#' @seealso \code{\link{numericInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetNumeric(list(inputId='obs', label='Observations:', value=10, min=1, max=100), lang=NULL)
widgetNumeric       <- function (param, lang) { widgetShiny(numericInput, param, lang) }

#' Encapsulates a Shiny \code{radioButtons}
#' 
#' Encapsulates a Shiny \code{radioButtons}. If you do not need a language change on-the-fly then you should use \code{radioButtons}.
#' 
#' @param param list of parameters for \code{radioButtons}
#' @param lang  language widget (or NULL)
#'
#' @seealso \code{\link{radioButtons}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetRadioButtons(list(inputId='dist', label='Distribution type:', 
#'                         choices=list("Normal" = "norm",
#'                                      "Uniform" = "unif", 
#'                                      "Log-normal" = "lnorm",
#'                                      "Exponential" = "exp")), 
#'                    lang=NULL)
widgetRadioButtons  <- function (param, lang) { widgetShiny(radioButtons, param, lang) }

#' Encapsulates a Shiny \code{selectInput}
#' 
#' Encapsulates a Shiny \code{selectInput}. If you do not need a language change on-the-fly then you should use \code{selectInput}.
#' 
#' @param param list of parameters for \code{selectInput}
#' @param lang  language widget (or NULL)
#'
#' @seealso \code{\link{selectInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetSelect(list(inputId="variable", label="Variable:",
#'                   choices=list("Cylinders" = "cyl",
#'                                "Transmission" = "am",
#'                                "Gears" = "gear")),
#'              lang=NULL)
widgetSelect <- function (param, lang) { widgetShiny(selectInput, param, lang) }

#' Encapsulates a Shiny \code{sliderInput}
#' 
#' Encapsulates a Shiny \code{sliderInput}. If you do not need a language change on-the-fly then you should use \code{sliderInput}.
#' 
#' @param param list of parameters for \code{sliderInput}
#' @param lang  language widget (or NULL)
#'
#' @seealso \code{\link{sliderInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetSlider(list(inputId='slider', label='My slider', min=0, max=10), lang=NULL)
widgetSlider        <- function (param, lang) { widgetShiny(sliderInput, param, lang) }

#' Encapsulates a Shiny \code{textInput}
#' 
#' Encapsulates a Shiny \code{textInput}. If you do not need a language change on-the-fly then you should use \code{textInput}.
#' 
#' @param param list of parameters for \code{textInput}
#' @param lang  language widget (or NULL)
#' 
#' @seealso \code{\link{textInput}}
#' 
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' widgetText(list(inputId='caption', label='Caption:', value="Data summary"), lang=NULL)
widgetText          <- function (param, lang) { widgetShiny(textInput, param, lang) }