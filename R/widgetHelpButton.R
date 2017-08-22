#' Widget for a Help Button
#'
#'
#' @param inputId widget name
#' @param url the html address regading the help page to display
#' @param lang language widget
#'
#' @seealso  \code{\link{actionButton}}
#'
#' @return a widget object (environment)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Press ESC after finishing the app
#
#' }
widgetHelpButton <- function(inputId,
                             url,
                             lang=NULL) {
  env               <- new.env()
  env[['widgetId']] <- inputId
  env[['lang']]     <- lang
  
  env[['ui']]$help <- list(func='actionButton',
                           args=mergeListsByName(sample,
                                                 list(inputId='help', 
                                                      label='Help',
                                                      onclick = paste0("window.open('", url, "', '_blank')"))))
  return(env)
}
