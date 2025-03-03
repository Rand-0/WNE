#' @export
marginaleffects <- function(model, characteristics) 
{
    model_check = list(model = model)
    class(model_check) = "wne_lt_model"
    validateObject(model_check)
    
    characteristics_check = list(characteristics = characteristics) 
    class(characteristics_check) = "wne_me_characteristics"
    validateObject(characteristics_check, model)
    
  if(is.list(characteristics))
  {
    characteristics = unlist(characteristics[order(match(names(characteristics),
                                                         names(model$coefficients)))])
  }
  
  # independent variables
  indeps = model$model[,-1]
  # dummy variables indices
  dvi = rep(0, times=ncol(indeps))
  for(i in 1:ncol(indeps)) {
    if(is.numeric(indeps[,i])==1) {
      if(length(unique(indeps[,i]))==2L) {
        # the variable is dummy
        dvi[i] = 1
      }
    }
  } 
  dvi = which(dvi==1)+1
  # plus one because of the constant in the model
  if(length(dvi)>0){ 
    x0 = characteristics
    x0[dvi] = 0
    x1 = x0
  }
  
  # Marginal effects
  if(model$family$link=="probit") {
    meff = as.vector(dnorm(characteristics%*%model$coefficients))*model$coefficients
    if(length(dvi)>0) {
      for(i in 1:length(dvi)) {
        x1[dvi[i]] = 1
        meff[dvi[i]] = pnorm(x1%*%model$coefficients)-pnorm(x0%*%model$coefficients)
        x1[dvi[i]] = 0
      }
    }
  } else if(model$family$link=="logit") {
    meff = as.vector(dlogis(characteristics%*%model$coefficients))*model$coefficients
    if(length(dvi)>0) {
      for(i in 1:length(dvi)) {
        x1[dvi[i]] = 1
        meff[dvi[i]] = plogis(x1%*%model$coefficients)-plogis(x0%*%model$coefficients)
        x1[dvi[i]] = 0
      }  
    }
  }
  meff = cbind(meff, characteristics)  
  colnames(meff) = c("Marginal effects", "at X=")
  if(length(dvi)>0) {
    rn = rownames(meff)
    for(i in 1:length(dvi)) {
      rn[dvi[i]] <- paste(rn[dvi[i]], "!", sep="")
    }
    rownames(meff) = rn
  }
  
  meff[1,1] = NA
  
  results = list(MEff = meff,
                 Dummies = ifelse(length(dvi)>0, T, F))
  
  class(results) = "MEff"
  
  results
  
}