source("./steps/data_analyzer.R")

x <- read.csv("./dataset/raw/X.txt", header = F, sep=" ")
c <- read.csv("./dataset/raw/C.txt", header = F, sep=" ")
y <- read.csv("./dataset/raw/Y.txt", header = F, sep=" ")

names(x) <- lapply(names(x), function(x) { gsub("V", "X", x) })
names(c) <- lapply(names(c), function(x) { gsub("V", "C", x) })
names(y) <- lapply(names(y), function(x) { gsub("V", "Y", x) })
x <- cbind(x,c)

# Split dataset
dataset <- data_split(x, y, percentace = 0.8, balanced = T)
train_dataset = list(x=dataset["x_train"], y=dataset["y_train"])
test_dataset = list(x=dataset["x_test"], y=dataset["y_test"])

# Analyze train dataset
data_analyzer(train_dataset["x"], train_dataset["y"])

# Clean train dataset
train_dataset <- data_cleaner(train_dataset["x"], train_dataset["y"])

# Augment train dataset
#train_dataset <- data_augmentation(train_dataset["x"], train_dataset["y"])

##### Now the train phase #####

# Feature select train dataset
train_dataset <- data_fselection(train_dataset["x"], train_dataset["y"])

# Normalize train dataset
train_dataset, normalizer <- data_normalization(train_dataset["x"], train_dataset["y"])

# Train model
train_metrics, model <- svm_train(train_dataset["x"], train_dataset["y"])

##### Now the test phase #####

# Feature select test dataset
test_dataset <- data_fselection(test_dataset["x"], test_dataset["y"])

# Normalize test dataset
test_dataset <- data_normalization(test_dataset["x"], test_dataset["y"], normalizer=normalizer)

# Test model
test_metrics <- svm_test(model, test_dataset["x"], test_dataset["y"])
