library(rjson)

generate_combinations_frame <- function(params) {
  combs <- expand.grid(params)
  names(combs) <- names(params)
  return(combs)
}

grid_search <- function(fun, params) { 
  print("Starting grid search")
  combinations <- generate_combinations_frame(params)
  print(paste("Combinations count:", dim(combinations)[1]))
  result <- apply(combinations, 1, function(x) { print(paste("Trying:", toJSON(as.list(x)))); do.call(fun, args=as.list(x))})
  result <- as.data.frame(result)
  rownames(result) <- apply(combinations, 1, function(x) { toJSON(as.list(x))})
  return(result)
}

#params <- list(a=c(1,2), b=c("aaa", "bbb"))
#params <- list(a=c(1,2), b=c("aaa", "bbb"), f=c("123", "555"))
#params <- list(kernel=c("linear", "radial", "polynomial", "sigmoid", "custom"), weight_1=c(1), weight_2=c(1))
#generate_combinations_frame(params)
#result <- grid_search(function(a, b) { print(paste("a: ", a, "; ", "b: ", b, sep="")) }, params)
