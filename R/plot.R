#' This contains the plot methods for linereg function.
#' 
#' 
#' @param x An object of linereg class
#' @param ... Further arguments passed to or from other methods
#' @import ggplot2
#' @export

plot.linreg <- function(x,y,...) 
{
   xrange=range(x$fitted)
   yrange=range(x$residuals)
   plot(xrange,yrange,main="Residuals vs Fitted",sub=x$call,xlab = "Fitted values",
        ylab ="Residuals")
   panel.smooth(x$fitted,x$residuals)
   sdres <- sd(residuals(x))
   ix<-which(abs(residuals(x))>2.5*sdres)
   text(fitted(x)[ix], residuals(x)[ix], ix, pos=2)
   par(ask=TRUE)
   stdresidual=sqrt(abs(x$residuals))
   yrange1=range(stdresidual)
   plot(xrange,yrange1,main="Scale-Location",sub=x$call,xlab = "Fitted values",
        ylab =expression(sqrt("|Standardized residuals|")))
   panel.smooth(x$fitted,stdresidual)
   sdres <- sd(stdresidual)
   ix<-which(abs(stdresidual)>4.3*sdres) 
   text(fitted(x)[ix], stdresidual[ix], ix, pos=2)
}


