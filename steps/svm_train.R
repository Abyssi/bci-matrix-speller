library("e1071")
library("kernlab")

svm_train <- function(x, y) {
  set.seed(123)
  
  #model <- svm(x=x, y=y, scale=F, type="C-classification", kernel="radial", probability = TRUE)
  #model <- svm(x=x, y=y, scale=F, type="C-classification", kernel="polynomial", probability = TRUE)
  #model <- svm(x=x, y=y, scale=F, type="C-classification", kernel="linear", cost=0.1, probability = TRUE)
  model <- svm(x=x, y=y, scale=F, type="C-classification", kernel="sigmoid", cost=2, probability = TRUE, class.weights = c("-1"=0.9, "1"=1), cachesize=500)
  #kfunction <- function(linear = 1, quadratic = 1)
  #{
  #  k <- function (x, y)
  #  {
  #    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  #  }
  #  class(k) <- "kernel"
  #  return(k)
  #}
  
  #model <- ksvm(as.matrix(x), as.matrix(y), scaled=F, type="C-svc", kernel=kfunction(1,0), C=0.1, prob.model=TRUE, class.weights = "inverse", cache=500)
  
  predictions <- predict(model, x, probability = TRUE)
  
  confusion_matrix <- table(predicted = predictions, observation = y)
  
  accuracy <- round((confusion_matrix["1","1"] + confusion_matrix["-1","-1"]) / nrow(x), 4)
  
  return(list(model = model, metrics=list(cm=confusion_matrix, accuracy = accuracy), output=predictions))
}