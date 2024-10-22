#' print
#'
#' @param x  
#' @param ... Additional arguments to be passed to or from methods.
#' @return An object
#' @export
print.linreg <- function(x,...) {
  cat("Call:", "\n")
  cat("linreg(formula = ", deparse(x$formula), ", data = ", x$name, ")\n\n", sep = "")
  cat("\ncoefficients:\n")
  W<- t(x$coefficients)
  print(W)
}
