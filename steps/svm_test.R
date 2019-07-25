library("e1071")

svm_test <- function(model, x, y=NULL) {
  set.seed(123)
  
  predictions <- predict(model, x, probability = TRUE)

  confusion_matrix <- if (!is.null(y)) table(predicted = predictions, observation = y) else NULL
  
  accuracy <- if (!is.null(y)) round((confusion_matrix["1","1"] + confusion_matrix["-1","-1"]) / nrow(x), 4) else NULL
  
  return(list(metrics=list(cm=confusion_matrix, accuracy = accuracy), output=predictions))
}