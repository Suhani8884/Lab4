#' This contains the regression coefficents of linreg function.
#' 
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

coef.linreg = function(object, ...){
  if(length(object$regression_coefficent)){
    print.default(t(object$regression_coefficent))
  } else {cat("Coefficient not available \n")}
}


