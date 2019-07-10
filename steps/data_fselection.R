derivative <- function (x) {
  return(diff(as.matrix(x), lag=1))
}

data_fselection <- function(data) {
  der <<- as.data.frame(derivative(data))
  names(der) <- lapply(names(der), function(x) { gsub("X", "D", x) })
  result <- cbind(data, der)
  return(result)
}