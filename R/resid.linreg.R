#' Residuals method

#' @param object An object of class 'linreg'.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A vector of residuals
#' @export
resid <- function(object,...){
  UseMethod("resid")
} 

#' @export
resid.linreg <- function(object,...) {
  return(object$residuals)
}

