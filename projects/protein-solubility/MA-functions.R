#Model Assessment code

format.accuracy.long <- function(data, dataset, method){
  # Formats the accuracy output from MC-functions.R/batch.bin
  #
  # Args:
  #   data - output of batch.bin $accuracy
  #   dataset - names of the dataset to include in the output dataframe
  #   method - which traning algorithms was used
  #
  # Returns:
  #   a dataframe with the following columns
  #   [1] "min"     "Q1"      "med"     "mean"    "Q2"      "max"     "folds"   "Measure" "Dataset"
  #   [10] "Method"
  #   Measure %in% c("Accuracy", "Sensitivity", "Specificity")
  #   Method %in% c("SVM", "randomForest", "ruleFit")
  #   The summary is computed for each fold, and dataset is from Args
  
  require(plyr)
  formatted <- as.data.frame(matrix(unlist(data), nrow=length(data), byrow=T))
  folds <- formatted[,1]
  acc <- formatted[,2:7]
  sens <- formatted[,8:13]
  spec <- formatted[,14:19]
  acc <- cbind(acc, folds)
  sens <- cbind(sens, folds)
  spec <- cbind(spec, folds)
  names(acc) <- c("min", "Q1", "med", "mean", "Q2", "max", "folds")
  names(sens) <- c("min", "Q1", "med", "mean", "Q2", "max", "folds")
  names(spec) <- c("min", "Q1", "med", "mean", "Q2", "max", "folds")
  acc$Measure <-"Accuracy"
  sens$Measure <- "Sensitivity"
  spec$Measure <- "Specificity"
  
  result <- rbind.fill(acc, sens, spec)
  result$Dataset <- dataset
  result$Method <- method
  return(result)
}

format.accuracy.wide <- function(data,dataset, method){
  # Formats the accuracy output from MC-functions.R/batch.bin
  # converts the list to a dataframe, and rename the columns
  #
  # Args:
  #   data - output of batch.bin $accuracy
  #   dataset - names of the dataset to include in the output dataframe
  #   method - which traning algorithms was used
  #
  # Returns:
  #   a dataframe with the following columns
  #   [1] "Fold"      "acc.min"   "acc.Q1"    "acc.med"   "acc.mean"  "acc.Q3"    "acc.max"  
  #   [8] "sens.min"  "sens.Q1"   "sens.med"  "sens.mean" "sens.Q3"   "sens.max"  "spec.min" 
  #   [15] "spec.Q1"   "spec.med"  "spec.mean" "spec.Q3"   "spec.max"  "Dataset"   "Method"  
  
  require(plyr)
  formatted <- as.data.frame(matrix(unlist(data), nrow=length(data), byrow=T))
  names(formatted) <- c("Fold", 
                        "acc.min", "acc.Q1", "acc.med", "acc.mean", "acc.Q3", "acc.max",
                        "sens.min", "sens.Q1", "sens.med", "sens.mean", "sens.Q3", "sens.max",
                        "spec.min", "spec.Q1", "spec.med", "spec.mean", "spec.Q3", "spec.max")
  formatted$Dataset <- dataset
  formatted$Method <- method
  return(formatted)
  
}

varImp.agg.mean <- function(models, measure){
  # Assessment of variable importance from randomForest models
  # 
  # Args:
  #   models - list of randomForest objects from different training runs
  #            output of batch.bin $models
  #   measure - which importance measure to use
  #             possible values c("MeanDecreaseAccuracy", "MeanDecreaseGini")
  #             when running from command line using Rscript, MeanDecreaseAccuracy is usually null
  # Returns:
  #   a dataframe reformatted to be used by plot.rf.imp.acc, or plot.rf.imp.gini

  require(randomForest)
  require(plyr)
  var.imp <- as.data.frame(lapply(models, importance))
  n <- length(models)
  m <- as.data.frame(apply(var.imp[, names(var.imp) %in% c(paste(measure, 1:n, sep="."), measure)], 1, summary))
  m.mean <- m["Mean", ]
  res <- as.data.frame(t(m.mean))
  
  switch(measure,
         "FALSE." = {names(res) = "False"},
         "TRUE." = {names(res) = "True"},
         "MeanDecreaseAccuracy" = {names(res) = "DecAcc"},
         "MeanDecreaseGini" = {names(res) = "DecGini"},
         )
  
  return(res)
}


#plots for summary of accuracy
plot.perf.sum <- function(data){
  # Plots performance summary of the model creation process
  #
  # Args:
  #   data - a dataframe, formatted by format.accuracy.long
  #
  # Returns:
  #   A plot with three panels; accuracy, sensitivity, and specificity
  #   x-axis: ratio of the training set
  #   y-axis: points from the summary of repeated trainings
  #   curves are smoothed, and grouped by the used algorithm
  #
  # Usage:
  #   all.acc <- rbind.fill(format.accuracy.long(model.acc.svm, data.name, "SVM"), 
  #                         format.accuracy.long(model.acc.rf, data.name, "RF"),
  #                         format.accuracy.long(model.acc.ruleFit, data.name, "RuleFit"))
  #   print(plot.perf.sum(all.acc))
  
  #data is output of format.accuracy.long
  require(ggplot2)
  data <- data[-4]
  data <- reshape(data, direction="long", 
                  idvar=c("folds", "Method", "Dataset", "Measure"), 
                  varying=1:5, v.names="Value")
  p <- ggplot(data,aes(x=1-1/folds, y=Value)) + 
    geom_smooth(aes(color=Method, shape=Method)) +
    geom_point(aes(color=Method, shape=Method))+
    xlab("Training") +
    ylab("Rate") +
    scale_y_continuous(limits = c(0, 1.1))+
    facet_wrap(~Measure, ncol=3)
  
  return(p)
  
}

plot.accuracy <- function(data){
  # Plots accuracy of the model creation process
  #
  # Args:
  #   data - a dataframe, formatted by format.accuracy.long
  #
  # Returns:
  #   A plot with one panel for accuracy
  #     x-axis: ratio of the training set
  #     y-axis: accuracy values from different models
  #     curves are smoothed, and grouped by the used algorithm
  #
  # Usage:
  #   all.acc <- rbind.fill(format.accuracy.long(model.acc.svm, data.name, "SVM"), 
  #                         format.accuracy.long(model.acc.rf, data.name, "RF"),
  #                         format.accuracy.long(model.acc.ruleFit, data.name, "RuleFit"))
  #   print(plot.accuracy(all.acc$Measure=="Accuracy",]))

  require(ggplot2)
  data <- data[-4]   #remove mean from the summary
  data <- reshape(data, direction="long", 
                  idvar=c("folds", "Method", "Dataset", "Measure"), 
                  varying=1:5, v.names="Value")
  p <- ggplot(data,aes(x=1-1/folds, y=Value)) + 
    geom_smooth(aes(color=Method, shape=Method)) +
    geom_point(aes(color=Method, shape=Method))+
    xlab("Training") +
    ylab("Accuracy") +
    scale_y_continuous(limits = c(0, 1.1))
  
  return(p)
  
}

plot.ROC <- function(data){
  # Plots ROC curve for the trained models
  #
  # Args:
  #   data - a dataframe, formatted by format.accuracy.wide
  #
  # Returns:
  #   comparison plot of ROC curves for differnt methods in the dataframe
  #
  # Usage:
  #   GH1.red.acc.wide <- rbind.fill(format.accuracy.wide(batch.GH1.red.ksvm$accuracy, "GH1", "SVM"), 
  #                                  format.accuracy.wide(batch.GH1.red.rf$accuracy, "GH1", "RF"))
  #   print(plot.ROC(GH1.red.acc.wide))
  
  require(ggplot2)
  p <- ggplot(data,aes(x=1-spec.mean, y=sens.mean)) + 
    geom_line(aes(color=Method, shape=Method)) +
    geom_point(aes(color=Method, shape=Method))+
    xlab("True Positive Rate") +
    ylab("False Positive Rate") +
    scale_y_continuous(limits = c(0, 1.1))
  
  return(p)
  
}

plot.rf.imp.acc <- function(data, top = 30, thr=FALSE){
  # Plots variable importance based on decreased accuracy from randomForest models
  #
  # Args:
  #   data - dataframe from varImp.agg.mean, with measure MeanDecreaseAccuracy
  #   top - how many variable/feature to include in the plot, default = 30
  #   thr - threshold for importance, the plot is generated for variables with 
  #         importance >= thr, currently not used
  #
  # Returns:
  #   A plot of variables/features ordered by importance for the MeanDecreaseAccuracy 
  #   measure
  #   x-axis: value for MeanDecreaseAccuracy
  #   y-axis: variable name, ordered bottom-up high-low
  #
  # Usage:
  #   model.rf.acc <- varImp.agg.mean(model.rf, "MeanDecreaseAccuracy")
  #   print(plot.rf.imp.acc(model.rf.acc))

  require(ggplot2)
  require(plyr)
  toplot.acc <- data
  toplot.acc$rn <- row.names(toplot.acc)
  
  toplot.acc <- toplot.acc[with(toplot.acc, order(DecAcc, decreasing=T)),]
  toplot.acc <- toplot.acc[1:top,]
  toplot.acc$obs <- factor(1:nrow(toplot.acc), labels=row.names(toplot.acc))
  toplot.acc$Measure <- "Acc"
  #print(toplot.gini)
  p <- ggplot(toplot.acc) +
    geom_point(aes(x=DecAcc, y=obs))+
    ylab("Feature")
  return(p)
}

plot.rf.imp.gini <- function(data, top = 30, thr=FALSE){
  # Plots variable importance based on decreased accuracy from randomForest models
  #
  # Args:
  #   data - dataframe from varImp.agg.mean, with measure MeanDecreaseGini
  #   top - how many variable/feature to include in the plot, default = 30
  #   thr - threshold for importance, the plot is generated for variables with 
  #         importance >= thr, currently not used
  #
  # Returns:
  #   A plot of variables/features ordered by importance for the MeanDecreasGini 
  #   measure
  #   x-axis: value for MeanDecreaseGini
  #   y-axis: variable name, ordered bottom-up high-low
  #
  # Usage:
  #   model.rf.gini <- varImp.agg.mean(model.rf, "MeanDecreaseGini")
  #   print(plot.rf.imp.gini(model.rf.gini))

  require(ggplot2)
  require(plyr)
  toplot.gini <- data
  toplot.gini$rn <- row.names(toplot.gini)
  
  toplot.gini <- toplot.gini[with(toplot.gini, order(DecGini, decreasing=T)),]
  toplot.gini <- toplot.gini[1:top,]
  toplot.gini$obs <- factor(1:nrow(toplot.gini), labels=row.names(toplot.gini))
  toplot.gini$Measure <- "Gini"
  #print(toplot.gini)
  p <- ggplot(toplot.gini) +
    geom_point(aes(x=DecGini, y=obs))+
    ylab("Feature")
  return(p)
}

#RuleFit functions
varImp.agg.mean.vi <- function(models){
  # Assessment of variable importance from ruleFit models
  # 
  # Args:
  #   models - list of ruleFit objects from different training runs
  #            output of batch.bin $models
  #
  # Returns:
  #   a dataframe reformatted to be used by plot.rf.imp.imp
  
  require(plyr)
  var.imp <- as.data.frame(lapply(models, function(x) x$vi))
  
  #print(names(var.imp))
  n <- length(models)
  
  m <- as.data.frame(apply(var.imp[, names(var.imp) %in% c(paste("imp", 1:n, sep="."), "imp")], 1, summary))
  
  m.mean <- m["Mean", ]
  
  res <- as.data.frame(t(m.mean))
  
  names(res) <- "imp"
  row.names(res) <- var.imp$nms
  return(res)
}

plot.rf.imp.imp <- function(data, top = 30, thr=FALSE){
  # Plots variable importance from ruleFit models
  #
  # Args:
  #   data - dataframe from varImp.agg.mean.vi
  #   top - how many variable/feature to include in the plot, default = 30
  #   thr - threshold for importance, the plot is generated for variables with 
  #         importance >= thr, currently not used
  #
  # Returns:
  #   A plot of variables/features ordered by importance 
  #   measure
  #   x-axis: value for Importance
  #   y-axis: variable name, ordered bottom-up high-low
  #
  # Usage:
  #   model.rf.imp <- varImp.agg.mean.vi(model.ruleFit)
  #   print(plot.rf.imp.imp(model.rf.imp))

  require(ggplot2)
  require(plyr)
  toplot.imp <- data
  toplot.imp$rn <- row.names(toplot.imp)
  
  toplot.imp <- toplot.imp[with(toplot.imp, order(imp, decreasing=T)),]
  toplot.imp <- toplot.imp[1:top,]
  toplot.imp$obs <- factor(1:nrow(toplot.imp), labels=row.names(toplot.imp))
  toplot.imp$Measure <- "Importance"
  p <- ggplot(toplot.imp) +
    geom_point(aes(x=imp, y=obs))+
    ylab("Feature")
  return(p)
}
