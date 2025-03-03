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
  if(is.list(object$characteristics))
  {
    if(!all(names(params$coefficients) %in% names(object$characteristics)))
    {
      stop('List of characteristics does not contain all of the parameters!', 
           call. = F)
    } else if(!all(names(object$characteristics) %in% names(params$coefficients)))
    {
      warning('Non-existent parametrs provided in characterist list has been suppressed!',
              call. = F)
    }
  } else if(is.vector(object$characteristics))
  {
    if(length(object$characteristics) != length(params$coefficients))
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