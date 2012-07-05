# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, folds=10, seed=NULL) {
  # Splits the data into two parts, training and testing
  # 
  # Args:
  #   dataframe - Dataframe to split
  #   folds - the number of partitions of the data, had k-folds been used
  #   seed - for the random generator of R
  #
  # Returns:
  #   a list l with two objects, l$trainset and l$testset of the same structure as dataframe
  #   size of l$testset is nrow(dataframe)/folds
  #   size of l$trainset is nrow(dataframe) (1/folds) 
  
  if (!is.null(seed)) set.seed(seed)
  data.size <- nrow(dataframe)
  index <- 1:data.size
  test.index <- sample(index, trunc(data.size/folds))
  #print(test.index)
  test.set <- dataframe[test.index,]
  train.set <- dataframe[-test.index,]
  return (list(trainset=train.set,testset=test.set))
}

split1out <- function(dataframe, index){
  test.set <- dataframe[index,]
  train.set <- dataframe[-index,]
  return (list(trainset=train.set,testset=test.set))
}

# splitK 
splitK <- function(dataframe, folds=10, seed=NULL, type="PROP", class="SOL.FLAG"){
  #splits the dataset into k folds
  #dataframe: input
  #folds: k
  #class: which variable to use as the descriminant
  #type: "P" proportionate, "B" balanced (size of the smallest set)
  var <- which(names(data)==class)
  values <- unique(dataframe[,c(var)])
  nc <- length(values)    #number of different classes
  np <- ceiling(nrow(dataframe)/folds)  #size of each partition
  
  for (v in values){
    #compute size of each class, np * proportion
    
  }
}

data.pre.var <- function(data, thr=0){
  # Preprocessess a data frame to remove variables with variance <= threshold
  #
  # Args:
  #   data - dataframe 
  #   thr - numeric, the value of the threshold
  #
  # Returns:
  #   a list of names for the variables where the variance is < = thr
  #
  # Usage:
  #   ex <- names(data) %in% data.pre.var(data)
  #   data <- data[-ex]
  data.var <- apply(data, 2, var)
  index <- which(data.var<=thr)
  return(names(index))
}

