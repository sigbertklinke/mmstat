#' generate, generateObject
#'
#' \code{generateObject} generates a object from \code{obj} with further parameters given in \code{...}. 
#' \code{generate} is just a shorthand for \code{generateObject}.
#'
#' @note There exists no default method to generatea new object. An error will be thrown.
#'
#' @param obj object
#' @param ... further to generate a new object from \code{obj} 
#'
#' @return a new object or \code{NULL} is generate was not success
#' @export
#'
#' @examples
#' x  <- rnorm(37)
#' ft <- frequencyTable(x)
#' generate(ft)
generate <- function (obj, ...) { generateObject(obj, ...) } # shorthand function

#' @rdname generate
generateObject <- function(obj, ...) UseMethod("generateObject")

#' @return \code{NULL}
#'
#' @rdname generate
#' @method generateObject default
#' @export 
generateObject.default <- function(obj, ...) {
  stop(sprintf("No default method for 'generate' or 'generateObject', class(es): %s", paste(class(obj), sep=",")))
}