library(rjson)

generate_combinations <- function(params) {
  if (length(params) == 0) return(c())
  if (length(params) > 1) {
    sub_combinations <- generate_combinations(params[2:length(params)]) 
    return(Reduce(function(...) rbind(...), lapply(params[[1]], function(x) { t(apply(sub_combinations,1, function(y) { print(c(x, y)) } )) } )))
  }
  else return(params[[1]])
}

generate_combinations_frame <- function(params) {
  combs <- generate_combinations(params)
  combs <- as.data.frame(combs)
  names(combs) <- names(params)
  rownames(combs) <- NULL
  return(combs)
}

grid_search <- function(fun, params) { 
  combinations <- generate_combinations_frame(params)
  print(paste("Combinations:", dim(combinations)[1]))
  result <- apply(combinations, 1, function(x) { print(paste("Trying:", toJSON(as.list(x)))); do.call(fun, args=as.list(x))})
  result <- as.data.frame(result)
  rownames(result) <- apply(combinations, 1, function(x) { toJSON(as.list(x))})
  return(result)
}

params <- list(a=c(1,2), b=c("aaa", "bbb"))
#params <- list(a=c(1,2), b=c("aaa", "bbb"), f=c("123", "555"))
#params <- list(kernel=c("linear", "radial", "polynomial", "sigmoid", "custom"), weight_1=c(1), weight_2=c(1))
generate_combinations(params)
#generate_combinations_frame(params)
result <- grid_search(function(a, b) { print(paste("a: ", a, "; ", "b: ", b, sep="")) }, params)
