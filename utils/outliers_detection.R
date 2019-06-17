outliers_detection <- function(attributes_names,dataset){
  set.seed(123)  
  atb <- dataset[[attributes_names]]
  outlier <- boxplot(atb, plot=F)$out
  n_outlier <- length(outlier)
  perc_outliers <- round(length(outlier)/nrow(dataset),4)
  
  output <- list(n_outlier,outlier,perc_outliers)
  names(output) <- c("Numero di outliers","outliers","Percentuale di outliers")
  return(output)
}