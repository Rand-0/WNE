#' @export
linktest <- function(model) 
{
    model_check = list(model = model)
    class(model_check) = "wne_lt_model"
    validateObject(model_check)
  
  #Linktest
  y = model$y
  yhat = log(model$fitted.values/(1 - model$fitted.values))
  yhat2 = yhat^2
  
  #Auxiliary regression
  aux.reg = glm(y~yhat+yhat2, family=binomial(link=model$family$link))
  
  results = list(coefficients = aux.reg$coefficients,
                 std.err = sqrt(diag(vcov(aux.reg))),
                 df = aux.reg$df.residual)
  
  class(results) = "linktest"
  
  return(results)
}