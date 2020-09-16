#' This contains the methods for linreg function.
#' 
#' The available methods are :
#' print(), plot(), resid(), pred(), coef() and summary()
#' 
#' 
#' @export

print.linreg = function(x,...){
  if(length(x$regression_coefficent)){
    cat("Call:\n ")
    print.default(as.vector(x$call))
    cat("\n Coefficent is \n")
    print.default(t(x$regression_coefficent))
  } else {cat("Coefficient not available \n")}
}


