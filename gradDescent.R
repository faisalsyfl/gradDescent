#Arranged by Faisal S.A
#22 October 2017
#Gradient Descent Algorithm

##
# Scaling every column using minmax Method
# @params {data.frames column} 
# @return {data.frames column} value of data.frames normalized
##
scalingColumn <- function(x){
  return((x-min(x)) / (max(x) - min(x)));  
}

##
# Feature Scaling data training.
# @params {data.frames} data frames that you want to be scaled
# @return {data.frame} data frames normalized
##
featureScaling <- function(x) {
  return (as.data.frame(lapply(x,scalingColumn)))
}


##
# Gradient Descent by Faisal S.A
# Learning Method to build a prediction mode
# @param {data.frames} dataTrain a data.frame that representing training data
# @param {num} alpha a float value representing learning rate.
# @param {num} maxIter the maximal number of iterations
# @param {num} seed a integer value for static random. Default value is NULL.
##
gradDescentFSA <- function(dataTrain, alpha = 0.1, maxIter = 15, seed = NULL) {
  numVar <- length(dataTrain);
  numRow <- nrow(dataTrain);
  set.seed(seed);
  tetha <- runif(numVar);
  for (i in 1:maxIter){
    temp <- tetha;
    for(j in 1:numVar){
      lastSum <- 0;
      for (k in 1:numRow){
        sum <- 0;
        for (l in 1:numVar) {
          if(l == 1){
            sum <- sum + tetha[l];
          }else{
            sum <- sum + (tetha[l]*dataTrain[k,l-1]);
            # print(dataTrain[k,l-1]);
          }
        }
        if(j != 1){
          sum <- sum * dataTrain[k,j-1];
        }
        lastSum <- lastSum + sum;
      }
      #sum y
      sumY <- sum(dataTrain[,ncol(dataTrain)]);
      
      tetha[j] <- temp[j] - ((alpha/numRow)*(lastSum-sumY));
    }
  }
  return(tetha);
}

