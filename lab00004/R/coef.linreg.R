#' Coefficients method
#'
#' @param X An object
#' @param ... Additional arguments to be passed to or from methods.
#' @return A named vector of coefficients
#' @export
coef.linreg <- function(X,...) {
  return(setNames(X$coefficients, paste0("Coefficient ", seq_along(X$coefficients))))
}
