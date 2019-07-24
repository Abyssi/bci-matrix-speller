library("CORElearn")

data_fselection <- function(input, labels, selector=NULL) {
  sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  
  data_split <- lapply(sensors, function (value) {
    columns <- lapply(as.list(1:floor(length(input)/length(sensors))), function(x) {paste(value, x, sep="_")})
    return(input[unlist(columns)])
  })
  names(data_split) <- sensors

  result <- lapply(data_split, function (value) {
    der <- as.data.frame(t(diff(t(as.matrix(value)), lag=1)))
    names(der) <- lapply(names(der), function(x) { return(paste("D", x, sep="_")) })
    return(der)
  })
  result <- Reduce(function(...) cbind(...), result)
  result <- cbind(input, result)
  
  if (is.null(selector)) {
    features <- attrEval(label ~ ., cbind(result, label=as.factor(labels)), estimator="ReliefFexpRank", kNearestExpRank=10, ReliefIterations=5)
    #features <- attrEval(label ~ ., cbind(result, label=as.factor(labels)), estimator="ReliefFequalK", kNearestExpRank=10, ReliefIterations=5)
    features <- sort(features, decreasing = T)
    features_subset <- names(features)[which(features >= 0)]
  }
  else {
    features_subset <- selector
  }

  result <- result[features_subset]
  
  return(list(selector=features_subset, output=result))
}