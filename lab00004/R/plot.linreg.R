#' Plot Residuals vs Fitted and Scale-Location
#'
#' This function generates two diagnostic plots for linear regression models.
#'
#' @param model A linear model object created by lm().
#' @name plot.linreg
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' linreg_mod <- lm(Petal.Length ~ Species, data = iris)
#' plot_diagnostics(linreg_mod)
#'
#' @export
library(ggplot2)
library(gridExtra)
linreg_mod <- lm(Petal.Length ~ Species, data = iris)
fitted_values <- fitted(linreg_mod)
residuals <- residuals(linreg_mod)
std_residuals <- rstandard(linreg_mod)
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
