#!/usr/bin/Rscript

#data input and cleaning
# All data files should be tab-separated


require(optparse)

option_list <- list( 
  make_option(c("-s", "--solubility"), 
              help="Solubility data file"),
  make_option("--sidvar", default="GeneID",
              help="Solubility file id variable [default %default]"),
  make_option("--svar", default="SOL",
              help="Solubility variable name [default %default]"),
  make_option("--sflag", default=FALSE,
              help="Solubility file id variable [default %default]"),
  make_option("--suthr", type="double",
              help="Solubility upper threshold for non binary data [default %default]"),
  make_option("--slthr", type="double",
              help="Solubility lower threshold for non binary data [default %default]"),
  make_option(c("-f", "--features"), 
              help="Features data file, example: output of pepstats or dimers"),
  make_option("--fidvar", default="ID",
              help="Features file id variable [default %default]"),
  make_option(c("-o", "--output"), default="out.csv",
              help = "Output file for clean data [default %default]")
  )

# args <- c("--solubility=aska-sol.tab", "--sidvar=JW_ID", "--svar=Solubility...",
#           "--suthr=70.0", "--slthr=30", 
#           "--features=Ecoli-K12/ASKA/freq.table", "--fidvar=gene_oid",
#           "--output=aska-dimers-out-thr.csv")

args <- c("--solubility=Ecoli-K12/Taghrid/GH1_expression_red.csv", "--sidvar=Gene.ID", 
          "--svar=sol", 
          "--suthr=0.2", "--slthr=0.2", 
          "--features=GH1/GH1-all.tab", "--fidvar=Gene.ID",
          "--output=GH1-all-out-thr.csv")

#args <- commandArgs(TRUE)

args.list <- parse_args(OptionParser(option_list = option_list), 
          args=args)
print(args.list)

data.sol <- read.delim(args.list$solubility)
print(names(data.sol))

data.features <- read.delim(args.list$features)
print(names(data.features))

data.all <- merge(data.sol, data.features, by.x=c(args.list$sidvar), by.y=c(args.list$fidvar))
print(names(data.all))

#threshold solubility and determine which variables to exclude
if (args.list$sflag == FALSE){
  # args.list$suthr or args.list$slthr is used
  # args.list$svar is numeric

  data.all[,c(args.list$svar)] <- ifelse(data.all[,c(args.list$svar)]>args.list$suthr, 1, 
                                   ifelse(data.all[,c(args.list$svar)]<args.list$slthr, -1,0))

  data.all <- data.all[data.all[,c(args.list$svar)]!=0,]

  data.all[,c(args.list$svar)] <- ifelse(data.all[,c(args.list$svar)]==1, TRUE, FALSE)

  data.all[,c(args.list$svar)] <- as.factor(data.all[,c(args.list$svar)])
  
#   data.all[,c(args.list$svar)] <- data.all[,c(args.list$svar)] > args.list$suthr
#   data.all[,c(args.list$svar)] <- as.factor(data.all[,c(args.list$svar)])
}else {
  # convert svar to boolean
  data.all[, c(args.list$svar)] <- as.logical(data.all[, c(args.list$svar)])
}

write.csv(data.all, args.list$output, row.names=FALSE)