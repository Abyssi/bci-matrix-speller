
data_load <- function(x_path, c_path, y_path) {
  x <- read.csv(x_path, header = F, sep=" ")
  c <- read.csv(c_path, header = F, sep=" ")
  y <- read.csv(y_path, header = F, sep=" ")
  
  sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  names(x) <-  mapply(function(value, index) { paste(sensors[1 + ((index - 1) %% length(sensors))], floor((index - 1)/length(sensors))+1, sep="_") }, names(x), seq_along(names(x)))
  names(c) <- lapply(names(c), function(x) { gsub("V", "C_", x) })
  names(y) <- lapply(names(y), function(x) { gsub("V", "Y_", x) })
  
  x_split <- lapply(sensors, function (value) {
    columns <- lapply(as.list(1:floor(length(x)/length(sensors))), function(x) {paste(value, x, sep="_")})
    return(x[, unlist(columns)])
  })
  
  names(x_split) <- sensors

  x <- cbind(x,c)
  return(list(x=x, c=c, y=y, x_split=x_split))
}