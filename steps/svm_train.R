svm_train <- function(x, y, model=class_svm()) {
  set.seed(123)
  
  model <- model(x, y)
  
  predictions <- predict(model, x, probability = TRUE)
  
  confusion_matrix <- table(predicted = predictions, observation = y)
  
  accuracy <- round((confusion_matrix["1","1"] + confusion_matrix["-1","-1"]) / nrow(x), 4)
  
  return(list(model = model, metrics=list(cm=confusion_matrix, accuracy = accuracy), output=predictions))
}

