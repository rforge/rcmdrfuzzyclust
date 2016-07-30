#' @export
checkManova<-function(clust){
  p<-ncol(clust$Clust.desc)
  data<-clust$Clust.desc[,1:p-1]
  cluster<-clust$Clust.desc[,p]
  fit<-manova(data~factor(cluster))
  manov<-summary(fit,test= c("Pillai"))
  return(manov$stats)
  }
