library("CORElearn")
library("signal")

data_fselection <- function(x_data, y_data, selector=NULL) {
  sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  
  data_split <- lapply(sensors, function (value) {
    columns <- lapply(as.list(1:floor(length(x_data)/length(sensors))), function(x) {paste(value, x, sep="_")})
    return(x_data[unlist(columns)])
  })
  names(data_split) <- sensors
  result <- as.data.frame(x_data)

  # Low pass filter
  res_lpf <- lapply(data_split, function (values) {
    fil <- as.data.frame(t(apply(values, 1, function(value) {
      return(filter(butter(2, 1/5, type="low"), unlist(value)))
    })))
    names(fil) <- lapply(names(values), function(x) { return(paste("F", x, sep="_")) })
    return(fil)
  })
  res_lpf <- Reduce(function(...) cbind(...), res_lpf)
  result <- cbind(res_lpf, x_data["C_1"])

  # Derivative
  #res_der <- lapply(data_split, function (value) {
  #  der <- as.data.frame(t(diff(t(as.matrix(value)), lag=1)))
  #  names(der) <- lapply(names(der), function(x) { return(paste("D", x, sep="_")) })
  #  return(der)
  #})
  #res_der <- Reduce(function(...) cbind(...), res_der)
  #result <- cbind(result, res_der)
  
  # Derivative
  #res_der <- lapply(data_split, function (value) {
  #  der <- as.data.frame(t(diff(t(as.matrix(value)), lag=1)))
  #  names(der) <- lapply(names(der), function(x) { return(paste("D", x, sep="_")) })
  #  return(der)
  #})
  #res_der <- Reduce(function(...) cbind(...), res_der)
  #result <- cbind(result, res_der)
  
  # Feature extraction
  if (is.null(selector)) {
    features <- attrEval(label ~ ., cbind(result, label=as.factor(y_data)), estimator="ReliefFexpRank", kNearestExpRank=100, ReliefIterations=5)
    #features <- attrEval(label ~ ., cbind(result, label=as.factor(y_data)), estimator="ReliefFequalK", kNearestExpRank=10, ReliefIterations=5)
    features <- sort(features, decreasing = T)
    features_subset <- names(features)[which(features >= 0)]
    #features_subset <- names(features)[1:1024]
  }
  else {
    features_subset <- selector
  }

  result <- result[features_subset]
  
  return(list(selector=features_subset, output=result))
}

#sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
#
#data_split <- lapply(sensors, function (value) {
#  columns <- lapply(as.list(1:floor(length(train_set$x)/length(sensors))), function(x) {paste(value, x, sep="_")})
#  return(train_set$x[unlist(columns)])
#})
#names(data_split) <- sensors

#b1 <- filtfilt(butter(2, 1/50, type="low"), data_split)

