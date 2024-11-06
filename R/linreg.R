#' Linear Regression using Ordinary Least Squares
#'
#' @param formula The formula.
#' @param data The data.
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
    Residual_standard_error = Residual_standard_error,
    rank = p,
    df_residual = df,  
    weights = rep(1, n),  
    qr = qr(X) 
  )
  class(result) <- "linreg"
  return(result)
}

#' rstandard method for linreg objects
#'
#' @param X An object of class 'linreg'.
#' @param ... Additional arguments (not used currently).
#' @return A vector of standardized residuals.
#' @export

rstandard <- function(X, ...) {
UseMethod("rstandard")
}
#' @export
#' @method rstandard linreg
rstandard.linreg <- function(X, ...) {
  residuals <- X$residuals
  hat_values <- lm.influence(X)$hat 
  sigma <- X$Residual_standard_error
  std_residuals <- X$residuals / (sigma * sqrt(1 - lm.influence(X)$hat))
  return(std_residuals)
}



#' Plot Residuals from Linear Regression
#'
#' This function takes a linear regression model and plots the residuals against the fitted values and the standardized residuals against the fitted values.
#'
#' @param x An object of class 'linreg', typically the result of a linear regression model.
#' @param ... Additional arguments passed to or from other methods.
#' @return Two ggplot objects are printed: 
#' 1. A plot of residuals vs fitted values.
#' 2. A plot of the square root of standardized residuals vs fitted values.
#' @export

plot.linreg <- function(x,...){
  fitted_values <- x$fitted_values
  residuals <- x$residuals
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
#' summary
#'
#' @param object 
#' @param ... Additional arguments to be passed to or from methods.
#' @return summary
#' @export
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
    } else {result$Significance[i] <- ""
    }
  }
  
  print(result, row.names = TRUE)
  
  cat("Residual standard error:", S5, "on", object$degrees_of_freedom,"degrees of freedom")
}

#' Coefficients method
#'
#' @param object An object
#' @param ... Additional arguments to be passed to or from methods.
#' @return A named vector of coefficients
#' @export
coef.linreg <- function(object,...) {
  return(setNames(object$coefficients, paste0("Coefficient ", seq_along(object$coefficients))))
}

#' Predicted values method
#'
#' @param object An object of class 'linreg'.
#' @return A vector of predicted values.
#' @param ... Additional arguments to be passed to or from methods.
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' @export
#' @method pred linreg
pred.linreg <- function(object, ...) {
  return(object$fitted_values)
}

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



