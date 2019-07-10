source("./steps/data_loader.R")
source("./steps/data_split.R")
source("./steps/data_analyzer.R")
source("./steps/svm_train.R")
source("./steps/svm_test.R")
source("./utils/bci_matrix_utils.R")

data <- data_load("./dataset/raw/X.txt", "./dataset/raw/C.txt", "./dataset/raw/Y.txt")
x = data$x
c = data$c
y = data$y

# Split dataset
dataset <- data_split(x, y, percentage = 0.8)
train_set <- list(x=dataset$x_train, y=dataset$y_train)
test_set <- list(x=dataset$x_test, y=dataset$y_test)
raw_train_set <- train_set
raw_test_set <- test_set

# Analyze train dataset
#data_analyze(train_set["x"], train_set["y"])

# Clean train dataset
source("./steps/data_cleaner.R")
train_set$x <- data_cleaner(train_set$x)

# Augment train dataset
#train_set <- data_augmentation(train_set["x"], train_set["y"])

##### Now the train phase #####

# Feature select train dataset
#train_set <- data_fselection(train_set["x"], train_set["y"])

# Normalize train dataset
source("./steps/data_normalization.R")
normalized <- data_normalization(train_set$x)
#train_set$x <- normalized$output
normalizer <- normalized$normalizer

# Train model
source("./steps/svm_train.R")
train_result <- svm_train(train_set$x, train_set$y)
model <- train_result$model
train_metrics <- train_result$metrics
word_train <- translate_to_word(train_set$x[,ncol(train_set$x)], train_set$y)
word_train

##### Now the test phase #####

# Feature select test dataset
#test_set <- data_fselection(test_set["x"], test_set["y"])

# Normalize test dataset
source("./steps/data_normalization.R")
#test_set$x <- data_normalization(test_set$x, normalizer)$output

# Test model
source("./steps/svm_test.R")
test_result <- svm_test(model, test_set$x, test_set$y)
word_a <- translate_to_word(test_set$x[,ncol(test_set$x)], test_set$y)
word_a
source("./utils/bci_matrix_utils.R")
result <- find_best_pairs(attr(test_result$output, "probabilities")[,1])
word_b <- translate_to_word(test_set$x[,ncol(test_set$x)], result)
word_b

source("./utils/bci_matrix_utils.R")
accuracy_by_char(word_a, word_b)
accuracy_by_row_column(test_set$y, result)

#print(test_result$output)
#print(as.integer(t(as.data.frame(test_result$output))))
#translate_to_word(test_set$x[,ncol(test_set$x)], as.integer(t(as.data.frame(test_result$output))))
