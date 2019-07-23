generate_combinations <- function(params) {
  if (length(params) == 0) return(c())
  sub_combinations <- if (length(params) > 1) generate_combinations(params[2:length(params)]) else c("")
  print(sub_combinations)
  return(apply(array(unlist(params[1])), 1, function(x) { apply(sub_combinations, 1, function(y) { c(x, y) } ) } ))
}

combs <- generate_combinations(params)
combs <- as.data.frame(combs)
names(combs) <- names(params)
combs

grid_search <- function(fun, params) { 
  combinations <- generate_combinations(params)
  return(lapply(combinations, function(x) { do.call(fun, x) }))
}