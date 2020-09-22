#' This contains the fitted values of linreg function.
#'
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

pred.linreg = function(object,...){
  if(length(object$fitted_values)){
    print.default(as.vector(object$fitted_values))
  } else {cat("Fitted values not available \n")}
}


