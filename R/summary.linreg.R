#' summary
#'
#' @param object 
#' @param ... Additional arguments to be passed to or from methods.
#' @return summary
#' @export
#' 
summary.linreg <- function(object,...) {

  S1 <- object$coefficients
  S2 <- object$standard_error
  S3 <- object$t_values
  S4 <- object$p_values
  S5 <- object$Residual_standard_error
  result <- data.frame(
    Coefficient = S1,
    Standard_Error = S2,
    t_value = S3,
    p_value = S4,
    Significance = character(length(S4)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(S4)) {
    if (S4[i] < 0.001) {
      result$Significance[i] <- "***"
    } else if (S4[i] < 0.01) {
      result$Significance[i] <- "**"
    } else if (S4[i] < 0.05) {
      result$Significance[i] <- "*"
    } else {
      result$Significance[i] <- ""
    }
  }

  print(result, row.names = TRUE)

  cat("Residual standard error:", S5, "on", object$degrees_of_freedom,"degrees of freedom")
}
