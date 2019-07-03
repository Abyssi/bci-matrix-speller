data_split <- function(x, y, percentage = 0.7){
  set.seed(123)
  
  train_size <- floor((percentage * nrow(x) / 120)) * 120

  x_train <- x[1:train_size,]
  y_train <- y[1:train_size,]
  
  x_test <- x[-train_size:-1,]
  y_test <- y[-train_size:-1,]
  
  return(list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test))
}