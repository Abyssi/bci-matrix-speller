library("e1071")

svm_train <- function(x, y) {
  set.seed(123)
  
  model <- svm(x=x, y=y, scale=F, type="C-classification", kernel="radial")
  
  predictions <- predict(model, x)
  
  confusion_matrix <- table(predicted = predictions, observation = y)
  
  accuracy <- round((confusion_matrix["1","1"] + confusion_matrix["-1","-1"]) / nrow(x), 4)
  
  return(list(model = model, metrics=list(cm=confusion_matrix, accuracy = accuracy)))
}