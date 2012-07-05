
#model creation
source("util.R")


batch.bin <- function(data, exclude.names = c("ID","SOL.EXP"), exclude.index=c(1), 
                      topredict="SOL.FLAG",
                      pred.p="TRUE",pred.n="FALSE", method="ksvm",
                      folds = c(2, 4, 6, 8, 10), 
                      repeats=10, debug=FALSE, ...){
  # Trains multiple models on the same dataset
  # 
  # Args:
  #   data - dataframe e.g. data.reduced
  #   exclude.names - features to exclude by name, column names to exclude from the learning
  #                   this parameter usually excludes constant data with zero variance
  #   exclude.index - features to exclude by column index
  #   topredict - name of the binary feature to predict, logical is assumed
  #   pred.p - the value of topredict that is considered positive sample
  #   pred.n - the value of topredict that is considered negative sample
  #   method - learning algorithms to use, 
  #            it can be any ofc("ksvm", "randomForest", "CTree", "ruleFit")
  #   folds -  number of different data partitions to try
  #   repeats - numbre of runs to perform for each fold value
  #   debug - print out debugging information
  #   ... - more parameters to pass to the learning algorithm, 
  #         example, kernel or type for ksvm
  #
  # Returns:
  #   a list l with two objects l$accuracy and l$models
  #   l$accuracy reports the summary of accuracy, sensitivity, and specificity
  #   for each fold across the number of repeats
  #   l$models reports a list of the models with maximum accuracy from each fold
  #
  # Usage:
  #   learn.bin <- batch.bin(data)   # default values used
  #   learn.bin$accuracy
  #   learn.bin$models
  
  #require(kernlab)
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
  
  rulefit.fun <- function(x, y, ...){
    newy <- ifelse(y==pred.p, 1, -1)
    #print(newy)
    rfmodel <- rulefit(as.matrix(x), newy, rfmode="class", ...)
    vi <- varimp(plot=F)
    vi$nms <- names(x[vi$ord])
    vi <- as.data.frame(vi)
    vi <- vi[order(vi$ord),]
    return(list(mod = rfmodel, vi=vi))
  }
  
  rulefit.predict <- function(newdata){
    p <- rfpred(as.matrix(newdata))
    #print("Predictions...")
    #print(p)
    ret <- ifelse(p>0, pred.p, pred.n)
    return(ret)
  }

  model.fun <- function(method, x, y, ...) {
    switch(method,
           ksvm = ksvm(as.matrix(x), y, ...),
           randomForest = randomForest(x, y, ...),
           ruleFit = rulefit.fun(x, y, ...),
           CTree = ctree(x, y, ...))
  }

  predict.fun <- function(method, object, newdata) {
    switch(method,
           ksvm = predict(object, newdata),
           randomForest = predict(object, newdata),
           ruleFit = rulefit.predict(newdata),
           CTree = predict(object, newdata))
  }
  
  rec <- NULL
  res <- vector("list", length(folds))
  models <- vector("list", length(folds))
  
  test.acc <- 1:repeats
  test.sens <- 1:repeats
  test.spec <- 1:repeats
  
  red <- data.pre.var(data)
  #if(debug) print(red)
  var.ex <- which(names(data) %in% red)
  if(debug){
    print(var.ex)
    print(class(var.ex))
  }
  
  exclude <- c(which(names(data)%in% exclude.names), exclude.index, var.ex)
  if (debug){
    print(exclude)
    print(class(exclude))
  }
  
  if (length(exclude) == 0){
    data.red <- data
  }else{
    data.red <- data[-exclude]
  }
  if (debug){
    print(names(data.red))
  }

  var <- which(names(data.red)==topredict)
  
  i <- 1
  for (fold in folds){
    if(debug) print(fold)
    final.model <- NULL
    final.acc <- 0 
    rec$fold <- fold
    for (rep in 1:repeats){
      if(debug) print(rep)
      test.splits <- splitdf(data.red, folds=fold)
      test.training <- test.splits$trainset
      test.testing <- test.splits$testset
#       test.model <- model.fun(method, formula(paste(topredict, "~.")), 
#                               data=test.training
#                               , ...)
      if (debug){
        print(nrow(test.training[c(-var)]))
        print(length(test.training[,c(var)]))
      }
      test.model <- model.fun(method, x=test.training[c(-var)], 
                              y=test.training[,c(var)]
                              , ...)
#      test.pred <- predict(test.model, test.testing[c(-var)])
      test.pred <- predict.fun(method, test.model, test.testing[c(-var)])
      test.acc[rep] <- nrow(test.testing[test.testing[,c(var)]==test.pred,]) /
        nrow(test.testing)
      test.sens[rep] <- nrow(test.testing[test.testing[,c(var)]==pred.p&test.pred==pred.p,])/
        nrow(test.testing[test.testing[,c(var)]==pred.p,])
      test.spec[rep] <- nrow(test.testing[test.testing[,c(var)]==pred.n&test.pred==pred.n,])/
        nrow(test.testing[test.testing[,c(var)]==pred.n,])
      if(debug) print(test.acc[rep])
      if (test.acc[rep]>final.acc){
        final.acc <- test.acc[rep]
        final.model <- test.model
        rm(test.model)
      }
      gc()
    }
    rec$acc <- summary(test.acc)
    rec$sens <- summary(test.sens)
    rec$spec <- summary(test.spec)
    if(debug) print(rec)
    if(debug) print(final.model)
    res[[i]] <- rec
    models[[i]] <- final.model
    i <- i+1
    rm(final.model)
    gc()
  }
  
  return(list(accuracy = res, models = models))
  
}
