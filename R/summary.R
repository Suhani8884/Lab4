#' This contains the summary method for linreg function.
#' 
#' 
#' @param object An object of linreg class
#' @param ... Further arguments passed to or from other methods
#' @export

summary.linreg = function(object,...){
    cat("Call:\n ")
    print.default(as.vector(object$call))
    a=cbind(as.matrix(object$regression_coefficent),as.matrix(sqrt(diag(object$variance_reg_coef))),as.matrix(object$t_value),as.matrix(object$p_value))
    colnames(a)=c("Estimate","std. Error","t value","p value")
    cat("\n Coefficents : \n")
    print.default(a)
    cat("\nResidual standard error:", sqrt(object$residual_variance), "on", object$degrees_of_freedom , "degrees of freedom")
}
