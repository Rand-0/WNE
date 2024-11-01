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