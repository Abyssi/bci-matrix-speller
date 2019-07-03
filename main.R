source("./steps/data_loader.R")
source("./steps/data_split.R")
source("./steps/data_analyzer.R")
source("./steps/svm_train.R")
source("./steps/svm_test.R")

data <- data_load("./dataset/raw/X.txt", "./dataset/raw/C.txt", "./dataset/raw/Y.txt")
x = data$x
c = data$c
y = data$y

# Split dataset
dataset <- data_split(x, y, percentage = 0.8)
train_set <- list(x=dataset$x_train, y=dataset$y_train)
test_set <- list(x=dataset$x_test, y=dataset$y_test)

# Analyze train dataset
#data_analyze(train_set["x"], train_set["y"])

# Clean train dataset
#train_set <- data_cleaner(train_set["x"], train_set["y"])

# Augment train dataset
#train_set <- data_augmentation(train_set["x"], train_set["y"])

##### Now the train phase #####

# Feature select train dataset
#train_set <- data_fselection(train_set["x"], train_set["y"])

# Normalize train dataset
#train_set, normalizer <- data_normalization(train_set["x"], train_set["y"])

# Train model
train_result <- svm_train(train_set$x, train_set$y)
model <- train_result$model
train_metrics <- train_result$metrics

##### Now the test phase #####

# Feature select test dataset
#test_set <- data_fselection(test_set["x"], test_set["y"])

# Normalize test dataset
#test_set <- data_normalization(test_set["x"], test_set["y"], normalizer=normalizer)

# Test model
test_metrics <- svm_test(model, test_set$x, test_set$y)
