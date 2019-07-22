data_fselection <- function(data, data_split) {
  result <- lapply(data_split, function (value) {
    der <- as.data.frame(t(diff(t(as.matrix(value)), lag=1)))
    names(der) <- lapply(names(der), function(x) { return(paste("D", x, sep="_")) })
    return(der)
  })
  result <- Reduce(function(...) cbind(...), train_set$aaa)
  #result <- cbind(data, result)
  return(result)
}