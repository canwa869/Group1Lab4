coef.linreg <-
function(X) {
  return(setNames(X$coefficients, paste0("Coefficient ", seq_along(X$coefficients))))
}
