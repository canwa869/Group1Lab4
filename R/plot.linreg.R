#' Plot Residuals from Linear Regression
#'
#' This function takes a linear regression model and plots the residuals
#' against the fitted values and the standardized residuals against the 
#' fitted values.
#'
#' @param x An object of class 'linreg', typically the result of a linear regression model.
#' @param ... Additional arguments passed to or from other methods.
#' @return Two ggplot objects are printed: 
#' 1. A plot of residuals vs fitted values.
#' 2. A plot of the square root of standardized residuals vs fitted values.
#' @name plot.linreg
#' @import ggplot2
#' @importFrom stats fitted residuals rstandard
#' @export
plot.linreg <- function(x,...){
  fitted_values <- fitted(x)
  residuals <- residuals(x)
  std_residuals <- rstandard(x)
  plot_data <- data.frame(fitted_values, residuals, std_residuals)
  plot_data$index <- 1:nrow(plot_data)
  p1 <- ggplot(plot_data, aes(x = fitted_values, y = residuals)) +
    geom_point(shape = 1,
               size = 2,
               color = "black") +
    geom_smooth(se = FALSE, color = "red") +
    geom_text(aes(label = ifelse(abs(residuals) > 1.1, index, "")),
              hjust = -0.2, vjust = -0.5) +
    labs(x = "Fitted values\nlm(Petal.Length ~ Species)",
         y = "Residuals", title = "Residuals vs Fitted") +
    theme_minimal()

  print(p1)
  p2 <- ggplot(plot_data, aes(x = fitted_values, y = sqrt(abs(std_residuals)))) +
    geom_point(shape = 1,
               size = 2,
               color = "black") +
    geom_smooth(se = FALSE, color = "red") +
    geom_text(aes(label = ifelse(sqrt(abs(std_residuals)) > 1.6, index, "")),
              hjust = -0.2, vjust = -0.5) +
    labs(x = "Fitted values\nlm(Petal.Length ~ Species)",
         y = expression(sqrt("Standardized residuals")), title = "Scale-Location") +
    theme_minimal()
  print(p2)
}
