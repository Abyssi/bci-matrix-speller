library("e1071")
library("kernlab")

kfunction <- function(linear = 1, quadratic = 1)
{
  k <- function (x, y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k) <- "kernel"
  return(k)
}

#linear, radial, polynomial, sigmoid, custom
class_svm <- function(kernel="linear", cost=1, weights=c("-1"=1, "1"=1)) {
  build <- function(x, y) {
    if (kernel == "custom")
      return(ksvm(as.matrix(x), as.matrix(y), scaled=F, type="C-svc", kernel=kfunction(1,0), C=cost, prob.model=TRUE, class.weights=weights, cache=500))
    else
      return(svm(x=x, y=y, scale=F, type="C-classification", kernel=kernel, cost=cost, probability=TRUE, class.weights=weights, cachesize=500))
  }
  return(build)
}