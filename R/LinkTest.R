#' @export
linktest <- function(object) 
{
    model_check = list(model = object)
    class(model_check) = "wne_lt_model"
    validateObject(model_check)
  
  #Linktest
  y = model$y
  yhat = log(model$fitted.values/(1 - model$fitted.values))
  yhat2 = yhat^2
  
  #Auxiliary regression
  aux.reg = glm(y~yhat+yhat2, family=binomial(link=model$family$link))
  show(summary(aux.reg))
  
  return(aux.reg)
}