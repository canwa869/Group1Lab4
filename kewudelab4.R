#' Linear Regression using Ordinary Least Squares
#'
#' @param X The design matrix (n x p) with n observations and p predictors.
#' @param y The response vector (n x 1) with n observations.
#' @return An object of class "linreg" containing many parameters.
#' @export


linreg <- function(formula, data){
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]] 
 # variables <- all.vars(formula)[-1] 
  name<- deparse(substitute(data))
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y_hat <- X %*% beta_hat
  e_hat <- y - y_hat
  n <- nrow(X)
  p <- ncol(X)
  df <- n-p
  sigma_square_hat <- as.numeric(t(e_hat) %*% e_hat / df)  #residual variance
  Residual_standard_error <- sqrt(sigma_square_hat)
  var_beta_hat <- sigma_square_hat * solve(t(X) %*% X)
  standard_error <- sqrt(diag(var_beta_hat))
  t_values <- beta_hat / (sqrt(diag(var_beta_hat)))
  p_values <- 2 * pt(-abs(t_values), df)
  qr_decomp <- qr(X)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)
  beta_hat1 <- solve(R) %*% t(Q) %*% y  #regression coefficient
  var_beta_hat1 <- sigma_square_hat * solve(t(R) %*% R)  #variance of regression coefficient
  result <- list(
    coefficients = beta_hat,
    fitted_values = y_hat,
    residuals = e_hat,
    degrees_of_freedom = df,
    residual_variance = sigma_square_hat,
    variance_coefficients = var_beta_hat,
    t_values = t_values,
    p_values = p_values,
    coefficients1 = beta_hat1,
    variance_coefficients1 = var_beta_hat1,
    formula = formula,
    data = data,
    name = name,
    standard_error = standard_error,
    Residual_standard_error = Residual_standard_error
  )
  class(result) <- "linreg"
  return(result)
}
print.linreg <- function(X) {
 
  W<- t(X$variance_coefficients)
  print(W)
}
data(iris)
mod_object <- linreg(Petal.Length~Species, data = iris)
print(mod_object)



#' print
#'
#' @param X 
#' @return An object
#' @export
print.linreg <- function(X) {
  cat("Call:", "\n")
  cat("linreg(formula = ", deparse(X$formula), ", data = ", X$name, ")\n\n", sep = "")
  cat("\ncoefficients:\n")
  W<- t(X$coefficients)
  print(W)
}
data(iris)
mod_object <- linreg(Petal.Length~Species, data = iris)
print(mod_object)


#' Plot Residuals from Linear Regression
#'
#' This function takes a linear regression model and plots the residuals
#' against the fitted values and the standardized residuals against the 
#' fitted values.
#'
#' @param linreg_mod A linear model object (of class `lm`).
#' 
#' @return Two ggplot objects are printed: 
#' 1. A plot of residuals vs fitted values.
#' 2. A plot of the square root of standardized residuals vs fitted values.
#' @name plot.linreg
#' @import ggplot2
#' @importFrom stats fitted residuals rstandard
#' 
#' @export
library(ggplot2)
library(gridExtra)
plot.linreg <- function(linreg_mod){
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
}
linreg_mod <- lm(Petal.Length ~ Species, data = iris)
plot.linreg(linreg_mod)

#' Residuals method
#'
#' @param X An object
#' @return A vector of residuals
#' @export
resid.linreg <- function(X) {
  return(X$residuals)
}

#' Predicted values method
#'
#' @param X An object
#' @return A vector of predicted values
#' @export
pred.linreg <- function(X) {
  return(X$fitted_values)
}

#' Coefficients method
#'
#' @param X An object
#' @return A named vector of coefficients
#' @export
coef.linreg <- function(X) {
  return(setNames(X$coefficients, paste0("Coefficient ", seq_along(X$coefficients))))
}

#' summary
#'
#' @param X 
#' @return summary
#' @export
#' 

summary.linreg <- function(X) {

  S1 <- X$coefficients
  S2 <- X$standard_error
  S3 <- X$t_values
  S4 <- X$p_values
  S5 <- X$Residual_standard_error
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
  
  cat("Residual standard error:", S5, "on", X$degrees_of_freedom,"degrees of freedom")
} 

data(iris)
mod_object <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
summary(mod_object)

package.skeleton("lab00004")
getwd()
setwd("C:/Users/0529y/Desktop/semester1/Advanced Programming in R/Git/lab04/lab00004")
usethis::use_description()

library(devtools)
use_testthat()
usethis::use_vignette("introduction")


devtools::install("C:/Users/0529y/Desktop/semester1/Advanced Programming in R/Git/lab04/lab00004")

browseVignettes("lab00004")

#' @import ggplot2
NULL
remotes::install_github("klutometis/roxygen")

install.packages("devtools")
library(devtools)
devtools::clean()
devtools::install() 
deps <- devtools::dev_package_deps()
print(deps)  
devtools::document()
devtools::build_vignettes()

