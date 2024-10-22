#' Predicted values method
#'
#' @param X An object
#' @return A vector of predicted values
#' @export
pred.linreg <-
  function(object) {
    return(object$fitted_values)
  }
