#' linreg
#' @param formula A formula.
#' @param data A data.
#' @description Implementing the calculations for multiple regression model using QR decomposition.
#' @return The function returns an object of class linreg as an S3 class
#' @export

linreg <-
  function(formula,data){
  X=model.matrix(formula,data)
  y=data[[all.vars(formula)[1]]]
  new_qr=qr(X)
  Q=qr.Q(new_qr)
  R=qr.R(new_qr)
  B=solve(R) %*% t(Q) %*% y 
  y1 = X %*% B 
  e = y-y1 
  df = nrow(X) - ncol(X) 
  v= sum((y-y1)^2)/df 
  v_rc = v * solve(t(R)%*% R) 
  t_value = as.numeric(B) / as.numeric(sqrt(diag(v_rc))) 
  p_value <- 2 * pt(t_value, df, lower = FALSE) 
  lst=list(B,y1,e,df,v,v_rc,t_value,p_value)
  linreg=structure(lst, class = "linreg")
  return(linreg)
  }
