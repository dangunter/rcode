#!/usr/bin/Rscript

#Model Creation driver

require(optparse)
source("util.R")
source("MC-functions.R")

option_list <- list( 
  make_option(c("-i", "--input"), 
              help="Input data file"),
  make_option("--delim", default=',', 
              help="Delimeter for the data input, [default %default]"),
  make_option(c("-d", "--idvar"), 
              help="ID variable in file"),
  make_option(c("-p", "--predict"),  
              help="Feautre to predict"),
  make_option(c("-m","--method"), default = "all",
              help = "Learning method; SVM, randomForest, [default %default]"),
  make_option("--excludeNames", default = "INCLUSION",
               help = "Variable names to exclude from learning [default %default]"),
  make_option("--excludeIndexes", default = c(1),
              help = "Variable indexes to exclude"),
  make_option(c("-o", "--output"), default = "out",
              help = "Output files prefix out.RData, and prefix for model names 
              out.ksvm, out.randomForest,...
              No special characters")
  )

args <- commandArgs(TRUE)
# args <- c("--input=out.csv", "--idvar=ID", "--predict=sol",
#           "--method=SVM",
#           "--output=GH1reducedSVM")
print(args)

args.list <- parse_args(OptionParser(option_list = option_list), 
           args = args)
#
print(args.list)

print("Reading data file ...")
data.features <- read.csv(args.list$input, sep=args.list$delim)
print(names(data.features))

data.features <- na.exclude(data.features)

data.features[,c(args.list$predict)] <- as.factor(data.features[,c(args.list$predict)])

print("Batch learning ...")

switch(args.list$method,
  "SVM" = {method = c("ksvm")},
  "randomForest" = {method = c("randomForest")},
  "ruleFit" = {method=c("ruleFit")},
  "all" = {method = c("ksvm", "randomForest", "ruleFit")}
  )

for (m in method){
  print(paste(m, "..."))
  assign(paste(args.list$output,m,sep="."),
         batch.bin(data.features, exclude.names=c(args.list$excludeNames),
                   exclude.index=c(args.list$excludeIndexes),
                   topredict=args.list$predict, method=m ))
  print(paste("Done", m))
  gc()
  save.image(paste(args.list$output, "RData", sep="."))
}

