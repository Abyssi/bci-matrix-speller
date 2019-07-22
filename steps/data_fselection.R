data_fselection <- function(data) {
  sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  
  data_split <- lapply(sensors, function (value) {
    columns <- lapply(as.list(1:floor(length(data)/length(sensors))), function(x) {paste(value, x, sep="_")})
    return(data[, unlist(columns)])
  })
  names(data_split) <- sensors
  data_split <<- data_split
  
  result <- lapply(data_split, function (value) {
    der <- as.data.frame(t(diff(t(as.matrix(value)), lag=1)))
    names(der) <- lapply(names(der), function(x) { return(paste("D", x, sep="_")) })
    return(der)
  })
  result <- Reduce(function(...) cbind(...), result)
  result <- cbind(data, result)
  return(result)
}