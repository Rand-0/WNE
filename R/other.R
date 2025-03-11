#'@export
atFUN <- function(FUN, ...)
{
  args = list(...)
  
  results = list(FUN = FUN,
                 args = args)
  
  class(results) = "wneFC"
  
  results
}