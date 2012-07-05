#!/usr/bin/Rscript

#Model Assessment driver
#usage
#Rscript MA-analysis.R workspace.RData svm-mode rf-model data-name output-name
#Rscript MA-analysis.R GH1-reduced.RData batch.GH1.red.ksvm batch.GH1.red.rf GH1 GH1-red

require(optparse)
require(plyr)
require(ggplot2)

theme_set(theme_bw())

option_list <- list( 
  make_option(c("-w", "--workspace"), 
              help="R Workspace file .RData, full path"),
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

args <- commandArgs(TRUE)
# args <- c("--workspace=GH1reducedSVM.RData", 
#           "--svm=GH1reducedSVM.ksvm", 
#           "--output=GH1reducedMCMA")

# args <- c("--workspace=onDash/batch-exclude/aska-reduced.RData", 
#           "--svm=batch.aska.red.ksvm", "--randomForest=batch.aska.red.rf", 
#           "--output=test-accuracy-print")

print(args)

args.list <- parse_args(OptionParser(option_list = option_list), 
                        args = args)
# 1: workspace name, from MC-analysis.R
# 2: svm-model name
# 3: rf-model name
# 4: string of data name
# 5: string for output file name, prescript of the graphics outputs

#workspace locations
#home.dir <- "~/Projects/protein-analysis/Data/onDash/batch-exclude/"
home.dir <- ""

ws <- paste(home.dir, args.list$workspace, sep="")
# ws.reduced <- paste(home.dir, c("GH1-reduced.RData","aska-reduced.RData"), sep="")
# ws.dimers <- paste(home.dir, c("GH1-dimers-rf.RData", "aska-dimers-rf.RData"), sep="")
# ws.all <- paste(home.dir, c("GH1-all-rf.RData", "aska-all-rf.RData"), sep="")

model.name.svm <- args.list$svm
model.name.rf <- args.list$randomForest
model.name.ruleFit <- args.list$ruleFit

#data names to use for outputs
data.name <- args.list$dataName
#output.file <- paste(home.dir, "GH1-red" ,sep="")
output.file <- paste(home.dir, args.list$output ,sep="")

#load the workspace: GH1 reduced as a test
print("Loading workspace ....")
load(ws)

source("MA-functions.R")
all.acc <- NULL

#process accuracy information
if (!is.null(model.name.svm)){
  model.acc.svm <- eval(as.name(model.name.svm))$accuracy
  all.acc <- rbind.fill(all.acc, format.accuracy.long(model.acc.svm, data.name, "SVM"))
} 

if (!is.null(model.name.rf)){
  model.acc.rf <- eval(as.name(model.name.rf))$accuracy
  all.acc <- rbind(all.acc, format.accuracy.long(model.acc.rf, data.name, "RF"))
  model.rf <- eval(as.name(model.name.rf))$models
  #print(model.rf)
  model.rf.gini <- varImp.agg.mean(model.rf, "MeanDecreaseGini")
  model.rf.acc <- varImp.agg.mean(model.rf, "MeanDecreaseAccuracy")
  #plot features
  
  pdf(paste(output.file, "-rf-varImp-gini.pdf", sep=""), width=5, height=5)
  print(plot.rf.imp.gini(model.rf.gini))
  dev.off()
  
  #does not work in command line with Rscript, need to figure out why
  pdf(paste(output.file, "-rf-varImp-acc.pdf", sep=""), width=5, height=5)
  print(plot.rf.imp.acc(model.rf.acc))
  dev.off()
} 

if (!is.null(model.name.ruleFit)){
  model.acc.ruleFit <- eval(as.name(model.name.ruleFit))$accuracy
  all.acc <- rbind.fill(all.acc, format.accuracy.long(model.acc.ruleFit, data.name, "RuleFit"))
  model.ruleFit <- eval(as.name(model.name.ruleFit))$models
  #print(model.rf)
  model.ruleFit.imp <- varImp.agg.mean.vi(model.ruleFit)
  #plot features
  
  print(paste(output.file, "-rf-vi.pdf", sep=""))
  pdf(paste(output.file, "-rf-vi.pdf", sep=""), width=5, height=5)
  print(plot.rf.imp.imp(model.ruleFit.imp))
  dev.off()
  
} 

#plot accuracy
# if ((!is.null(model.name.svm)) && (!is.null(model.name.rf)) &&
#   (!is.null(model.name.ruleFit))) {
#   all.acc <- rbind.fill(format.accuracy.long(model.acc.svm, data.name, "SVM"), 
#                           format.accuracy.long(model.acc.rf, data.name, "RF"),
#                           format.accuracy.long(model.acc.ruleFit, data.name, "RuleFit"))
# } else if (!is.null(model.name.svm)){
#   all.acc <- format.accuracy.long(model.acc.svm, data.name, "SVM")
# } else if (!is.null(model.name.rf)){
#   all.acc <- format.accuracy.long(model.acc.rf, data.name, "RF")
# } else if (!is.null(model.name.ruleFit)){
#   all.acc <- format.accuracy.long(model.acc.ruleFit, data.name, "RuleFit")
# }
#print(all.acc)

pdf(paste(output.file, "-accuracy.pdf", sep=""), width=8, height=4)
print(plot.perf.sum(all.acc))
dev.off()

sink(file=paste(output.file,"output", sep="."), append=T)
print(all.acc)
sink()


