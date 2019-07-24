data_split <- function(x, y, percentage = 0.7) {
  set.seed(123)
  
  train_size <- floor((percentage * nrow(x) / 120)) * 120

  x_train <- x[1:train_size,]
  y_train <- y[1:train_size,]
  
  x_test <- x[-train_size:-1,]
  y_test <- y[-train_size:-1,]
  
  return(list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test))
}

n_cross_fold <- function(x, y, n, func, i=-1) {
  print(paste("Starting", n, "cross fold validation"))
  folds <- cut(seq(1, nrow(x)), breaks=n, labels=FALSE)
  lapply(if(i== -1) 1:n else i:i, function(i) {
    print(paste("Split index:", i))
    testIndexes <- which(folds==i, arr.ind=TRUE)
    x_train <- x[-testIndexes, ]
    y_train <- y[-testIndexes, ]
    
    x_test <- x[testIndexes, ]
    y_test <- y[testIndexes, ]
    return(func(list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test)))
  })
}
