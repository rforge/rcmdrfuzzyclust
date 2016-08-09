#' Preparing data for clustering.
#'
#' @description This function used to construct data for clustering from
#' dataset with chosen variables.
#' @details Don't use it from user.
#' @param var.choice Chosen Variables of Dataset
#' @return data.cluster Dataset with chosen variables
#' @export


managedata<-function(var.choice){
  data.cluster<-matrix(nrow=nrow(data))
  for(i in var.choice)
    data.cluster<-cbind.data.frame(data.cluster,eval(parse(text=paste("data$",i,sep=""))))
  data.cluster<-data.cluster[,-1]
  colnames(data.cluster)<-var.choice
  return(data.cluster)
}
