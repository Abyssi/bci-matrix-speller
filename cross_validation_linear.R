cross_validation_linear <- function(train_x,train_y,cost,k_fold,weight){
  library(caret)
  source("classification_parameters.R")
  football_data <- cbind(train_x,train_y)
  #creazione di 10 coppie trainign(70%) test(30%) bilanciate 
  partitions <- createDataPartition(football_data$train_y,times = k_fold,p=0.7)
  
  f_classification <- function(idx,data,c,wgh){
    train <- data[idx,1:(ncol(data)-1) ]
    test <- data[-idx, 1:(ncol(data)-1) ]
    ytrain <- data[idx, ncol(data)]
    ytest <- data[-idx,ncol(data)]
    
    scaled_training <- scale(train,center = T,scale = T)
    
    
    model <- LiblineaR(data= scaled_training,target=ytrain,wi = wgh,cost=c,
                 type=1)
    TR_prediction <- predict(model,scaled_training)
    TR_confusionmatrix <- table(TR_prediction,ytrain)
    TR_result <- classification_parameters(TR_confusionmatrix,train)
    
    scaled_test <- scale(test,attr(scaled_training,"scaled:center"),attr(scaled_training,"scaled:scale"))
    TS_prediction <- predict(model,scaled_test)
    TS_confusionmatrix <- table(TS_prediction,ytest)
    TS_result <- classification_parameters(TS_confusionmatrix,test)
    
    output <- list(TR_result,TS_result)
    names(output) <- c("TR_results","TS_results")
    return(output)
    
  }
  
  
  cls_result <- lapply(cost,function(x){
    performance <-lapply(partitions, f_classification,data=football_data,hyperparameter=x,wgh=weight)
    return(performance)
  })
  names(cls_result) <- cost
  return(cls_result)
  
  
  
}