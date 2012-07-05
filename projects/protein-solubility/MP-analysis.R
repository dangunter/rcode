#!/usr/bin/Rscript

require(optparse)
require(plyr)
require(ggplot2)

theme_set(theme_bw())

option_list <- list( 
  make_option(c("-i", "--input"), 
              help="Input data file"),
  make_option("--delim", default=',', 
              help="Delimeter for the data input, [default %default]"),
  make_option(c("-d", "--idvar"), 
              help="ID variable in file"),
  make_option(c("-p", "--predict"),  
              help="Feautre to predict"),
  make_option(c("-w", "--workspace"), 
              help="R Workspace file .RData, full path to the workspace containing the models"),
  make_option("--svm", 
              help="SVM model name in RData workspace"),
  make_option("--randomForest",  
              help="RandomForest model name in RData workspace"),
  make_option("--ruleFit",  
              help="RuleFit model name in RData workspace"),
  make_option("--dataName", default = "data",
              help = "String for data name [default %default]"),
  make_option(c("-o", "--output"), default = "data",
              help = "String for output file prefixes [default %default]")
  )

#args <- commandArgs(TRUE)
# args <- c("--input=aska-out-thr.csv",
#           "--idvar=JW_ID", "--predict=Solubility...",
#           "--workspace=onDash/batch-exclude/aska-reduced.RData",
#           "--svm=batch.aska.red.ksvm",  "--randomForest=batch.aska.red.rf",
#           "--output=aska-aska-red-predict")

# args <- c("--input=GH1-dimers-out-thr.csv",
#           "--idvar=Gene.ID", "--predict=sol",
#           "--workspace=onDash/batch-exclude/aska-dimers-rf.RData",
#           "--svm=batch.aska.dimers.ksvm",  "--randomForest=batch.aska.dimers.rf",
#           "--output=GH1-aska-dimers-predict")

args <- c("--input=GH1-all-out-thr.csv",
          "--idvar=Gene.ID", "--predict=sol",
          "--workspace=onDash/batch-exclude/aska-all-rf.RData",
          "--svm=batch.aska.all.ksvm",  "--randomForest=batch.aska.all.rf",
          "--output=GH1-aska-all-predict")

# args <- c(
#   "--input=out.csv", "--idvar=Gene.ID", "--predict=sol",
#   "--workspace=onDash/batch-exclude/GH1-reduced.RData", 
#   "--svm=batch.GH1.red.ksvm", "--randomForest=batch.GH1.red.rf",  
#           "--output=test-predict-GH1")
print(args)

args.list <- parse_args(OptionParser(option_list = option_list), 
                        args = args)

print("Reading data file ...")
data.features <- read.csv(args.list$input, sep=args.list$delim)
print(names(data.features))

data.features <- na.exclude(data.features)

data.features[,c(args.list$predict)] <- as.factor(data.features[,c(args.list$predict)])

print("Loading model workspace ...")
home.dir <- ""

ws <- paste(home.dir, args.list$workspace, sep="")

model.name.svm <- args.list$svm
model.name.rf <- args.list$randomForest
model.name.ruleFit <- args.list$ruleFit

#data names to use for outputs
data.name <- args.list$dataName
#output.file <- paste(home.dir, "GH1-red" ,sep="")
output.file <- paste(home.dir, args.list$output ,sep="")

#load the workspace: GH1 reduced as a test
load(ws)

source("MP-functions.R")

print(data.features[,args.list$predict])

sink(file=args.list$output)
sink()

if (!is.null(args.list$predict)){
  
  if (!is.null(model.name.svm)){
    models.svm <- eval(as.name(model.name.svm))$models
    acc.svm <- batch.model.perf(data.features, models.svm, args.list$predict)
    print("SVM performance ...")
    sink(file=args.list$output, append=T)
    print(format.model.perf(acc.svm))
    mis.svm <- batch.model.mis(data.features, models.svm, yvar=args.list$predict, idvar=args.list$idvar)
    print(mis.svm)
    sink()
  } 
  
  if (!is.null(model.name.rf)){
    models.rf <- eval(as.name(model.name.rf))$models
    acc.rf <- batch.model.perf(data.features, models.rf, args.list$predict, method="randomForest")
    print("SVM performance ...")
    sink(file=args.list$output, append=T)
    print(format.model.perf(acc.rf))
    mis.rf <- batch.model.mis(data.features, models.rf, yvar=args.list$predict, idvar=args.list$idvar,
                              method="randomForest")
    print(mis.rf)
    sink()
  } 
  
} else{
  if (!is.null(model.name.svm)){
    models.svm <- eval(as.name(model.name.svm))$models
    newy.svm <- llply(.data=models.svm, .fun=predict.bin, data=data.features)
    sink(file=args.list$output, append=T)
    print(newy.svm)
    sink()
  } 
  
  if (!is.null(model.name.rf)){
    models.rf <- eval(as.name(model.name.rf))$models
    newy.rf <- llply(.data=models.rf, .fun=predict.bin, data=data.features, method="randomForest")
    sink(file=args.list$output, append=T)
    print(newy.rf)
    sink()
  } 
  
}

save.image(paste(args.list$output, "RData", sep="."))

