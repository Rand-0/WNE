#' @export
print.linktest <- function(object) 
{
  values <- data.frame(
    Estimate = object$coefficients,
    std.err = object$std.err)
  
  values$tval <- round(values$Estimate / values$std.err, 3)
  
  values$pval <- 2 * (1 - pt(abs(values$tval), object$df))
  values$pval <- ifelse(values$pval < 1e-3, format(values$pval, scientific = FALSE, digits = 3), round(values$pval, 3))
  
  values$Stars <- sapply(values$pval, function(p) {
    if (as.numeric(p) < 0.001) return(" ***")
    else if (as.numeric(p) < 0.01) return(" **")
    else if (as.numeric(p) < 0.05) return(" *")
    else if (as.numeric(p) < 0.1) return(" .")
    else return(" ")
  })
  
  cat("      Linktest\n\n")
  cat("Coefficients:\n")
  cat(sprintf("%-15s %10s %10s %10s %12s\n", "", 
              "Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  
  
  row_names <- c("(Intercept)", "_hat", "_hatsq")
  for (i in seq_along(row_names)) {
    cat(sprintf("%-15s %10.5f %10.5f %10.3f %12s%s\n", 
                row_names[i], 
                values$Estimate[i], 
                values$std.err[i], 
                values$tval[i], 
                values$pval[i], 
                values$Stars[i]))
  }
  
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' @export
print.MEff <- function(object)
{
  print(object$MEff, na.print = ".")
  if(object$Dummies)
  {
    cat("---\n")
    cat("(!) indicates marginal effect was calculated for discrete change of dummy\n")
    cat("variable from 0 to 1\n")
  } else
  {
    cat("\n")
  }
}
