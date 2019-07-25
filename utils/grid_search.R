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
