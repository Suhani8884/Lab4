#' This contains the print methods for linreg function.
#' 
#' 
#' @param x An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

print.linreg = function(x,...){
  if(length(x$regression_coefficent)){
    cat("Call:\n ")
    print.default(as.vector(x$call))
    cat("\n Coefficent is \n")
    print.default(t(x$regression_coefficent))
  } else {cat("Coefficient not available \n")}
}


