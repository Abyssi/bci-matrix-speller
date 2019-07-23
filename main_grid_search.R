# Load dataset
source("./steps/data_loader.R")
data <- data_load("./dataset/raw/X.txt", "./dataset/raw/C.txt", "./dataset/raw/Y.txt")
x <- data$x
c <- data$c
y <- data$y

# Split dataset
source("./steps/data_split.R")
dataset <- data_split(x, y, percentage = 0.8)
train_set <- list(x=dataset$x_train, y=dataset$y_train)
test_set <- list(x=dataset$x_test, y=dataset$y_test)
raw_train_set <- train_set
raw_test_set <- test_set

pipeline <- function(kernel="linear", cost=1, weights=c("-1"=1, "1"=1)) {
  ##### Now the train set pipeline #####
  
  # Clean train dataset
  source("./steps/data_cleaner.R")
  train_set$x <- data_cleaner(train_set$x)
  
  # Feature select train dataset
  source("./steps/data_fselection.R")
  train_set$x <- data_fselection(train_set$x)
  
  # Normalize train dataset
  source("./steps/data_normalization.R")
  normalized <- data_normalization(train_set$x)
  train_set$x <- normalized$output
  normalizer <- normalized$normalizer
  
  ##### Now the test set pipeline #####
  
  # Feature select test dataset
  source("./steps/data_fselection.R")
  test_set$x <- data_fselection(test_set$x)
  
  # Normalize test dataset
  source("./steps/data_normalization.R")
  test_set$x <- data_normalization(test_set$x, normalizer)$output
  
  ##### Now the model definition #####
  model <- class_svm(kernel=kernel, cost=cost, weights=weights)
  
  ##### Now the train phase #####
  
  # Train model
  source("./steps/svm_train.R")
  train_result <- svm_train(train_set$x, train_set$y)
  model <- train_result$model
  
  ##### Now the test phase #####
  
  # Test model
  source("./steps/svm_test.R")
  test_result <- svm_test(model, test_set$x, test_set$y)
  
  #Return metrics
  return(train_metrics=train_result$metrics, test_metrics=test_result$metrics)
}

source("./utils/grid_search.R")
grid_search(pipeline, list(kernel=c("linear", "radial", "polynomial", "sigmoid", "custom"), cost=c(1:10), weights=c(c("-1"=1, "1"=1))))

