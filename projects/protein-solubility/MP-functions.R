# Model prediction functions from external datasets

predict.bin <- function(model, data, method="ksvm"){
  # Predicts the classification of data objects from model
  # Only features included in model will be used in the prediction
  # 
  # Args:
  #   model - model object from the training
  #   data - dataframe e.g. data.reduced
  #   method - learning algorithms to use, 
  #            it can be any of c("ksvm", "randomForest", "CTree", "ruleFit")
  #            CTree and ruleFit are not implemented yet
  #
  # Returns:
  #   a vector of predictions
  #

  require(plyr)
  
  switch(method,
         ksvm = require(kernlab),
         randomForest = require(randomForest),
         CTree = require(party),
         ruleFit = {
           #platform = "linux"
           #rfhome = "/home/taghrid/R/rfhome"
           source("/home/taghrid/R/rfhome/rf-setup.R")
           #install.packages("akima", lib=rfhome)
           #library(akima, lib.loc=rfhome) 
         }
         )
  
  rulefit.predict <- function(newdata){
    p <- rfpred(as.matrix(newdata))
    #print("Predictions...")
    #print(p)
    ret <- ifelse(p>0, pred.p, pred.n)
    return(ret)
  }
  
  predict.fun <- function(method, object, newdata) {
    switch(method,
           ksvm = predict(object, newdata),
           randomForest = predict(object, newdata),
           ruleFit = rulefit.predict(newdata),
           CTree = predict(object, newdata))
  }

  switch(method,
         ksvm = {vars <- names(data) %in% names(as.data.frame(xmatrix(model)))},
         randomForest = {vars <- names(data) %in% rownames(importance(model))}
    )
  

  test.pred <- predict.fun(method, model, data[vars])
  
  return(test.pred)
}

model.perf <- function(y, newy, pred.p="TRUE", pred.n="FALSE"){
  # Computes accuracy, sensitivity, and specificity for y
  #
  # Args:
  #   y - vector of reference values
  #   newy - predicted value to compare
  #   pred.p - the value of y that is considered positive sample
  #   pred.n - the value of y that is considered negative sample
  # 
  # Returns:
  #   a list l with the following items
  #   l$acc - accuracy
  #   l$sens - sensitivity
  #   l$spec - specificity
  
  res <- NULL
  res$acc <- table(y==newy)[c("TRUE")]/ length(y)
  res$sens <- table(y==pred.p & newy==pred.p)[c("TRUE")] / table(y==pred.p)[c("TRUE")] 
  res$spec <- table(y==pred.n & newy==pred.n)[c("TRUE")] / table(y==pred.n)[c("TRUE")]

  return(res)
}

batch.model.perf <- function(data, models, yvar, method="ksvm"){
  # Apply models to the dataset, and return performance measures for each model
  # 
  # Args:
  #   data - new dataset to predict
  #   models - list of model objects generated, currently supports ksvm and randomForest
  #   yvar - which variable to predict, has to be the same as the created model
  #   method - which model class to use, c("ksvm", "randomForest")
  #  
  # Returns:
  #   a list of accuracy measures for each model
  #
  # Usage:
  #   acc <- batch.model.perf(data.features, GH1reducedSVM.ksvm$models, "sol")
  
  require(plyr)
  newy <- llply(.data=models, .fun=predict.bin, data=data, method=method)
  accuracy <- llply(.data = newy, .fun=model.perf, y=data[,c(yvar)])
  return(accuracy)
}

format.model.perf <- function(data){
  # Format data for printing
  #
  # Args:
  #   data - list, output from batch.model.perf
  #
  # Returns:
  #   dataframe, each row corresponds to a model from the data
  #   c("Accuracy", "Sensitivity", "Specificity")
  
  d <- as.data.frame(matrix(unlist(data), nrow=length(data), byrow=T))
  names(d) <- c("Accuracy", "Sensitivity", "Specificity")
  return(d)
}

batch.model.mis <- function(data, models, yvar, idvar, method="ksvm"){
  # Apply models to the dataset, and return common mis-classified proteins
  # 
  # Args:
  #   data - new dataset to predict
  #   models - list of model objects generated, currently supports ksvm and randomForest
  #   yvar - which variable to predict, has to be the same as the created model
  #   method - which model class to use, c("ksvm", "randomForest")
  #  
  # Returns:
  #   a vector of object ids from data that were mis-classified at all models

  require(plyr)
  newy <- llply(.data=models, .fun=predict.bin, data=data, method=method)
  mis <- llply(.data = newy, .fun=function(x) which(x!=data[,c(yvar)]))
  lres <- as.data.frame(1:nrow(data))
  names(lres) <- c("index")
  for (l in mis) {
    li <- as.data.frame(l)
    names(li) <- c("index")
    lres <- merge(lres, li)
  }
  
  return(data[lres$index, c(idvar)])
}
