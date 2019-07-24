library("car")

data_analyze <- function(x, y) {
  set.seed(123)
  
  plot_folder <- paste(getwd(), "plot/", sep="/" )
  if (!dir.exists(plot_folder)) {
    dir.create(plot_folder)
  }
  
  sensors <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "PO7", "PO8")
  
  #divide senors by columns
  x_split <- lapply(sensors, function (value) {
    columns <- lapply(as.list(1:floor(length(x)/length(sensors))), function(x) {paste(value, x, sep="_")})
    return(x[unlist(columns)])
  })
  names(x_split) <- sensors
  
  #create plots
  plot(unlist(x_split$Fz[1,]), type="l", col="black", main="Fz[1]", xlab="samples", ylab="value")
  densityPlot(unlist(x_split$Fz[1,]), col="black", main="Fz[1] density", xlab="value", ylab="density")
}