validateObject <- function(object, params = NULL, params2 = NULL)
{
  UseMethod("validateObject")
}

#' @export
validateObject.wne_lt_model <- function(object, params = NULL, params2 = NULL)
{
  if(!any(class(object$model) == "glm"))
  {
    stop('Provided argument should be a fitted "glm" object!', call. = F)
  }
}

#' @export
validateObject.wne_me_characteristics <- function(object, params = NULL, 
                                                  params2 = NULL)
{
  if(any(class(params) %in% c("glm","polr")))
  {
    model_params = params$coefficients
  } else if(any(class(params) == "censReg"))
  {
    model_params = params$estimate[-length(params$estimate)]
  }
  
  if(is.list(object$characteristics))
  {
    if(!all(names(model_params) %in% names(object$characteristics)))
    {
      stop('List of characteristics does not contain all of the parameters!', 
           call. = F)
    } else if(!all(names(object$characteristics) %in% names(model_params)))
    {
      warning('Non-existent parametrs provided in characterist list has been suppressed!',
              call. = F)
    }
  } else if(is.vector(object$characteristics))
  {
    if(length(object$characteristics) != length(model_params))
    {
      stop('Vector of characteristics does not match parameters of the model!',
           call. = F)
    } 
  } else
  {
    stop('Characteristics should be provided as a vector or named list!',
         call. = F)
  }
}

#'@export
validateObject.wne_me_dummies <- function(object, params = NULL, params2 = NULL)
{
  coef = names(object$model$estimate[-c(1, length(object$model$estimate))])
  dummies = object$dummies
  
  if(!is.character(dummies))
  {
    stop('Dummy.vars should be an character vector!', call. = F)
  }
  
  if(!all(dummies %in% coef))
  {
    stop('At least one element of dummies.indicies is not a model parameter!', 
         call. = F)
  }
}
