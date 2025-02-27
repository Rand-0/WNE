#' @export
linktest <- function(object) 
{
    model_check = list(model = object)
    class(model_check) = "wne_lt_model"
    validateObject(model_check)
  
  #Linktest
  y = object$y
  yhat = log(object$fitted.values/(1 - object$fitted.values))
  yhat2 = yhat^2
  
  #Auxiliary regression
  aux.reg = glm(y~yhat+yhat2, family=binomial(link=object$family$link))
  
  results = list(coefficients = aux.reg$coefficients,
                 std.err = sqrt(diag(vcov(aux.reg))),
                 df = aux.reg$df.residual)
  
  class(results) = "linktest"
  
  return(results)
}