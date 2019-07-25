#load dataset 
data_load <- function(x_path, c_path, y_path=NULL) {
  x <- read.csv(x_path, header = F, sep=" ")
  c <- read.csv(c_path, header = F, sep=" ")
  y <- if (!is.null(y_path)) read.csv(y_path, header = F, sep=" ") else NULL
  
  #menage index
  sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  names(x) <-  mapply(function(value, index) { paste(sensors[1 + ((index - 1) %% length(sensors))], floor((index - 1)/length(sensors))+1, sep="_") }, names(x), seq_along(names(x)))
  names(c) <- lapply(names(c), function(x) { gsub("V", "C_", x) })
  if (!is.null(y_path)) names(y) <- lapply(names(y), function(x) { gsub("V", "Y_", x) })
  
  #append c as last column of x
  x <- cbind(x,c)
  
  return(list(x=x, c=c, y=y))
}