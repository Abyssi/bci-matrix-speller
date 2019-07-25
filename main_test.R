pipeline <- function(train_set, test_set, kernel="linear", cost=1, weight_no=1, weight_yes=1) {
  print("Starting pipeline")
  raw_train_set <- train_set
  raw_test_set <- test_set
  ##### Now the train set pipeline #####
  
  # Clean train set
  print("Clean train set")
  source("./steps/data_cleaner.R")
  train_set$x <- data_cleaner(train_set$x)
  
  # Feature select train set
  print("Feature select train set")
  source("./steps/data_fselection.R")
  selected <- data_fselection(train_set$x, train_set$y)
  train_set$x <- selected$output
  selector <- selected$selector
  
  # Normalize train set
  print("Normalize train set")
  source("./steps/data_normalization.R")
  normalized <- data_normalization(train_set$x)
  train_set$x <- normalized$output
  normalizer <- normalized$normalizer
  
  processed_train_set <<- train_set
  
  ##### Now the test set pipeline #####
  
  # Feature select test set
  print("Feature select test set")
  source("./steps/data_fselection.R")
  test_set$x <- data_fselection(test_set$x, test_set$y, selector)$output
  
  # Normalize test set
  print("Normalize test set")
  source("./steps/data_normalization.R")
  test_set$x <- data_normalization(test_set$x, normalizer)$output
  
  processed_test_set <<- test_set
  
  ##### Now the model definition #####
  
  # Define the model
  print("Define the model")
  source("./utils/class_svm.R")
  model <- class_svm(kernel=kernel, cost=cost, weights=c("-1"=weight_no, "1"=weight_yes))
  
  ##### Now the train phase #####
  
  # Train model
  print("Train model")
  source("./steps/svm_train.R")
  train_result <- svm_train(model, train_set$x, train_set$y)
  model <- train_result$model
  
  # Evalutate model on train set
  print("Evalutate model on train set")
  source("./utils/bci_matrix_utils.R")
  train_p <- find_best_pairs(attr(train_result$output, "probabilities")[,1])
  train_word <- translate_to_word(raw_train_set$x[,ncol(raw_train_set$x)], train_set$y)
  train_word_p <- translate_to_word(raw_train_set$x[,ncol(raw_train_set$x)], train_p)
  
  train_metrics <- list(metrics=train_result$metrics, 
                      accuracy_by_char=accuracy_by_char(train_word, train_word_p),
                      train_word=train_word,
                      train_word_p=train_word_p)
  
  ##### Now the test phase #####
  
  # Test model
  print("Test model")
  source("./steps/svm_test.R")
  test_result <- svm_test(model, test_set$x)
  
  # Evalutate model on test set
  print("Evalutate model on test set")
  source("./utils/bci_matrix_utils.R")
  test_p <- find_best_pairs(attr(test_result$output, "probabilities")[,1])
  test_word_p <- translate_to_word(raw_test_set$x[,ncol(raw_test_set$x)], test_p)
  
  return(test_word_p)
}

# Load dataset
source("./steps/data_loader.R")
train_data <- data_load("./dataset/raw/X.txt", "./dataset/raw/C.txt", "./dataset/raw/Y.txt")
test_data <- data_load("./dataset/raw/X.txt", "./dataset/raw/C.txt")

# Define dataset
train_set <- list(x=train_data$x, y=unlist(train_data$y))
test_set <- list(x=test_data$x, y=NULL)

word <- pipeline(train_set=train_set, test_set=test_set, kernel="linear", cost=0.01, weight_no=1, weight_yes=1)
print(paste("The word is:", word))

