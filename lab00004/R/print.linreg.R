#' print
#'
#' @param X 
#' @param ... Additional arguments to be passed to or from methods.
#' @return An object
#' @export
print.linreg <- function(X,...) {
  cat("Call:", "\n")
  cat("linreg(formula = ", deparse(X$formula), ", data = ", X$name, ")\n\n", sep = "")
  cat("\ncoefficients:\n")
  W<- t(X$coefficients)
  print(W)
}