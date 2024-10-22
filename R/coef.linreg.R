#' Coefficients method
#'
#' @param X An object
#' @param ... Additional arguments to be passed to or from methods.
#' @return A named vector of coefficients
#' @export
coef.linreg <- function(object,...) {
  return(setNames(object$coefficients, paste0("Coefficient ", seq_along(object$coefficients))))
}
