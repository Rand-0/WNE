#' @export
marginaleffects <- function(model, characteristics, ...) 
{
  UseMethod("marginaleffects")
}

#' @export
marginaleffects.glm <- function(model, characteristics, ...) 
{
  if(class(characteristics) == "wneFC")
  {
    FUN = match.fun(characteristics$FUN)
    args = characteristics$args
    
    if(length(args) == 0)
    {
      temp = apply(model$model[-1], 2, FUN)
    } else
    {
      temp = apply(model$model[-1], 2, FUN, unlist(args))
    }
    
    if(length(temp) == (length(model$coefficients) - 1))
    {
      warning("Provided atFUN did not return value for Intercept, so it was replaced by 1.",
              call. = F)
      characteristics = c(1, temp)
    } else if(length(temp) != length(model$coefficients))
    {
      stop("Provided atFUN did not return enough values to replace characteristics!",
           call. = F)
    }
  }
  
  
    characteristics_check = list(characteristics = characteristics) 
    class(characteristics_check) = "wne_me_characteristics"
    validateObject(characteristics_check, model)  
  
  
    model_check = list(model = model)
    class(model_check) = "wne_lt_model"
    validateObject(model_check)
  
  
  if(is.list(characteristics))
  {
    characteristics = unlist(characteristics[order(na.omit(match(names(characteristics),
                                                         names(model$coefficients))))])
  }
  
  # independent variables
  indeps = model$model[-1]
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
  
  x0 = characteristics
  
  # plus one because of the constant in the model
  if(length(dvi)>0){ 
    x0[dvi] = 0
  }
  
  x1 = x0
  
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
  meff = cbind(meff, x0)  
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
                 Dummies = ifelse(length(dvi)>0, T, F),
                 Type = "Logit/Probit marginal effects")
  
  class(results) = "MEff"
  
  results
}

#' @export
marginaleffects.censReg <- function(model, characteristics, ...) 
{
  if(class(characteristics) == "wneFC")
  {
    stop("Option atFUN is currently not avaiable for Tobit models!",
         call. = F)
  }
  
    characteristics_check = list(characteristics = characteristics) 
    class(characteristics_check) = "wne_me_characteristics"
    validateObject(characteristics_check, model)  
  
  
  dummies = unlist(list(...)["dummy.vars"])
  
  if(!is.null(dummies))
  {
    dummies_check = list(model = model,
                         dummies = dummies)
    class(dummies_check) = "wne_me_dummies"
    validateObject(dummies_check)
  }
  
  
  if(is.list(characteristics))
  {
    characteristics = unlist(characteristics[order(na.omit(match(names(characteristics),
                                                                 names(model$estimate))))])
  }
  
  x = characteristics
  dvi = match(dummies, names(characteristics))
  
  # estimates without ln(Sigma)
  beta_hat = as.vector(model$estimate[-length(model$estimate)])
  sigma_hat = model$estimate[length(model$estimate)]
  
  # Marginal Effects: Probability Uncensored
  me_prob_uncens = dnorm(t(x)%*%beta_hat/exp(sigma_hat))%*%beta_hat/exp(sigma_hat)
  
  # Marginal Effects: Unconditional Expected Value
  me_uncond_evalue = pnorm(t(x)%*%beta_hat/exp(sigma_hat))%*%beta_hat
  
  # Marginal Effects: Conditional on being Uncensored
  # Xbeta_hat/Sigma xBdS
  xBdS = t(x)%*%beta_hat/exp(sigma_hat)
  me_cond_uncens = (1-dnorm(xBdS)/pnorm(xBdS)*(xBdS+dnorm(xBdS)/pnorm(xBdS)))%*%beta_hat

  if(length(dvi)>0){ 
    x0 = x
    x0[dvi] = 0
    x1 = x0
    
    for(i in 1:length(dvi)) {
      x1[dvi[i]] = 1
      # Marginal Effects: Probability Uncensored
      me_prob_uncens[i] = pnorm(t(x1)%*%beta_hat/exp(sigma_hat))-pnorm(t(x0)%*%beta_hat/exp(sigma_hat))
      
      # Marginal Effects: Conditional on being Uncensored
      # Xbeta_hat/Sigma xBdS
      xBdS1 = t(x1)%*%beta_hat/exp(sigma_hat)
      xBdS0 = t(x0)%*%beta_hat/exp(sigma_hat)
      me_cond_uncens[i] = (t(x1)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS1)/pnorm(xBdS1))-
        (t(x0)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS0)/pnorm(xBdS0))
      
      # Marginal Effects: Unconditional Expected Value
      me_uncond_evalue[i] = (t(x1)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS1)/pnorm(xBdS1))*pnorm(xBdS1)-
        (t(x0)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS0)/pnorm(xBdS0))*pnorm(xBdS0)
      x1[dvi[i]] = 0
    }
  }
  
  # Marginal effects brief table
  varnames = names(model$estimate[-length(model$estimate)])
  names(beta_hat) = varnames
  me = cbind(beta_hat, t(me_uncond_evalue), t(me_cond_uncens), t(me_prob_uncens), x)
  colnames(me) <- c("y*", "E(y|x)", "E(y|x,y>0)", "Pr(y>0|x)", "at X=")
  
  if(length(dvi)>0) {
    rn = rownames(me)
    for(i in 1:length(dvi)) {
      rn[dvi[i]] <- paste(rn[dvi[i]], "!", sep="")
    }
    rownames(me) = rn
  }
  
  me[1,1:(ncol(me)-1)] = NA
  
  results = list(MEff = me,
                 Dummies = ifelse(length(dvi)>0, T, F),
                 Type = "Tobit marginal effects")
  
  class(results) = "MEff"
  
  results
}


#' @export
marginaleffects.polr <- function(model, characteristics, ...) 
{
  if(class(characteristics) == "wneFC")
  {
    FUN = match.fun(characteristics$FUN)
    args = characteristics$args
    
    if(length(args) == 0)
    {
      temp = apply(model$model[-1], 2, FUN)
    } else
    {
      temp = apply(model$model[-1], 2, FUN, unlist(args))
    }
    
    if(length(temp) != length(model$coefficients))
    {
      stop("Provided atFUN did not return enough values to replace characteristics!",
           call. = F)
    }
    
    characteristics = temp
  }
  
    characteristics_check = list(characteristics = characteristics) 
    class(characteristics_check) = "wne_me_characteristics"
    validateObject(characteristics_check, model)  
  
  
  if(is.list(characteristics))
  {
    characteristics = unlist(characteristics[order(na.omit(match(names(characteristics),
                                                                 names(model$coefficients))))])
  }
  
  x = characteristics
  
  
  ff = function(model, value) {
    if (model$method=="logistic") {
      value = dlogis(value)
    } else if (model$method=="probit") {
      value = dnorm(value)
    }
    return(value)
  }
  
  fcdf = function(model, value) {
    if (model$method=="logistic") {
      value = plogis(value)
    } else if (model$method=="probit") {
      value = pnorm(value)
    }
    return(value)
  }
  
  # model$method
  y = as.matrix(sapply(model$model[,1], as.numeric))
  list_of_alternatives = sort(unique(y))
  J = length(list_of_alternatives)
  beta_hat = model$coefficients
  cutoffs = model$zeta
  
  names = model$lev
  
  # the very first alternative
  meff = t(-ff(model, cutoffs[1]-x%*%beta_hat)%*%beta_hat)
  # middle alternatives
  for(i in 2:(J-1)) {
    me = t(-ff(model, cutoffs[i]-x%*%beta_hat)%*%beta_hat+ff(model, cutoffs[i-1]-x%*%beta_hat)%*%beta_hat)
    meff = cbind(meff, me)
  }
  # the last alternative
  me = t(ff(model, cutoffs[J-1]-x%*%beta_hat)%*%beta_hat)
  meff = cbind(meff, me)
  
  # dummy variables marginal effects
  # independent variables
  indeps = model$model[-1]
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
  dvi = which(dvi==1)
  
  x0 = x
  
  if(length(dvi)>0){ 
    x0[dvi] = 0
  }
  
  x1 = x0
  
  # Marginal effects of the dummy variables
  if(length(dvi)>0) {
    for(i in 1:length(dvi)) {
      for(j in 1:J) {
        x1[dvi[i]] = 1
        if(j==1){
          meff[dvi[i], j] = fcdf(model, cutoffs[j]-x1%*%model$coefficients)-
            fcdf(model, cutoffs[j]-x0%*%model$coefficients)
        } else if(j==J){
          meff[dvi[i], j] = (1-fcdf(model, cutoffs[J-1]-x1%*%model$coefficients))-
            (1-fcdf(model, cutoffs[J-1]-x0%*%model$coefficients))
        } else {
          meff[dvi[i], j] = fcdf(model, cutoffs[j]-x1%*%model$coefficients)-
            fcdf(model, cutoffs[j-1]-x1%*%model$coefficients)-
            fcdf(model, cutoffs[j]-x0%*%model$coefficients)+
            fcdf(model, cutoffs[j-1]-x0%*%model$coefficients)
        }
        x1[dvi[i]] = 0
      }
    }  
  }
  
  rownames(meff) = attr(model$coefficients, "names")
  if(length(dvi)==0){ 
    x0 = x
  }
  meff = cbind(meff, x0)  
  colnames(meff) = c(names, "at X=")
  if(length(dvi)>0) {
    rn = rownames(meff)
    for(i in 1:length(dvi)) {
      rn[dvi[i]] <- paste(rn[dvi[i]], "!", sep="")
    }
    rownames(meff) = rn
  }
  
  results = list(MEff = meff,
                 Dummies = ifelse(length(dvi)>0, T, F),
                 Type = "Ordered logit/probit marginal effects")
  
  class(results) = "MEff"
  
  results
}

