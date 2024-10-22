#' Residuals method
#'
#' @param X An object
#' @param ... Additional arguments to be passed to or from methods.
#' @return A vector of residuals
#' @export
resid <- function(object,...){
  UseMethod("resid")
} 
resid.linreg <- function(object,...) {
  return(object$residuals)
}

