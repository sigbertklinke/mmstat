#' Render a conditional panel
#'
#' @param inputId input element for the condition
#' @param ... parameter sets for the widget to render
#' @param lang language widget (or NULL)
#' @param session session parameter of the server function
#'
#' @seealso \code{\link{conditionalPanel}}
#'
#' @return HTML code which can be used instead of \code{renderUI}
#' @export
#'
#' @examples
#' \dontrun{
#'   demo(mmstatDiscreteDistributions)
#' }
renderPanel <- function(inputId, ..., session=NULL) {
  widgets <- list(...)
  ret     <- list()
  for (i in seq(length(widgets))) {
    ret[[i]] <- conditionalPanel(sprintf("input['%s']==%i", inputId, i),
                                 renderWidget(widgets[[i]], session=session)
                                 )  
  }
  return(ret)
}