#' Residuals method
#'
#' @param X An object
#' @return A vector of residuals
#' @export
resid.linreg <-
function(X) {
  return(X$residuals)
}
