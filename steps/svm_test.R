library("e1071")

svm_test <- function(model, x, y) {
  set.seed(123)
  
  predictions <- predict(model, x, probability = TRUE)

  confusion_matrix <- table(predicted = predictions, observation = y)
  
  accuracy <- round((confusion_matrix["1","1"] + confusion_matrix["-1","-1"]) / nrow(x), 4)
  
  return(list(metrics=list(cm=confusion_matrix, accuracy = accuracy), output=predictions))
}