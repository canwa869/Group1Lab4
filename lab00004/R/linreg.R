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