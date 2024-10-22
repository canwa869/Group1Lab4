#' print
#'
#' @param X 
#' @param ... Additional arguments to be passed to or from methods.
#' @return An object
#' @export
print.linreg <- function(object,...) {
  cat("Call:", "\n")
  cat("linreg(formula = ", deparse(object$formula), ", data = ", object$name, ")\n\n", sep = "")
  cat("\ncoefficients:\n")
  W<- t(object$coefficients)
  print(W)
}
