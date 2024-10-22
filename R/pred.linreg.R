#' Predicted values method
#'
#' @param object An object
#' @return A vector of predicted values
#' @export
pred.linreg <-
  function(object) {
    return(object$fitted_values)
  }
