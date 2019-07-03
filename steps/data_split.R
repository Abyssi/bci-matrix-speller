data_split <- function(x, y, percentage = 0.7){
  set.seed(123)
  
  data <- cbind(x, y)

  output <- by(data, as.factor(data$Y1), function(x){
    smp_size <- floor(percentage * nrow(x))
    train_ind <- sample(seq_len(nrow(x)), size = smp_size)
    train <- x[train_ind,]
    test <- x[-train_ind,]
    return(list(train=train, test=test))
  })
  
  train_set <- rbind(output$`1`$train, output$`-1`$train)
  test_set <- rbind(output$`1`$test, output$`-1`$test)

  #shuffle row-wise
  train_set <- train_set[sample(nrow(train_set)),]
  test_set <- test_set[sample(nrow(test_set)),]
  
  x_train <- train_set[,1:(ncol(train_set)-1)]
  y_train <- train_set[,ncol(train_set)]
  
  x_test <- test_set[,1:(ncol(test_set)-1)]
  y_test <- test_set[,ncol(test_set)]
  
  return(list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test))
}