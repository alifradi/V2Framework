CramerSelection <- function(data, target){
  features <- names(data[,names(data) != target])
  x<-cramer(data, target, features)
  df = data.frame(Features=names(x), CramerV=x)
  return(df)
}

BestSubsetSelection_k <- function(data,target , k){
  x<-selectKBest(data, target, roughsetConsistency, k = k)
  x1<-unlist(x$featuresSelected)
  x2<-unlist(x$valuePerFeature)
  df <- data.frame(Feature = x1, ValuePerFeature = x2)
  return(df)
}
