data_load <- function(x_path, c_path, y_path) {
  x <- read.csv(x_path, header = F, sep=" ")
  c <- read.csv(c_path, header = F, sep=" ")
  y <- read.csv(y_path, header = F, sep=" ")
  
  names(x) <- lapply(names(x), function(x) { gsub("V", "X", x) })
  names(c) <- lapply(names(c), function(x) { gsub("V", "C", x) })
  names(y) <- lapply(names(y), function(x) { gsub("V", "Y", x) })
  x <- cbind(x,c)
  
  return(list(x=x, c=c, y=y))
}