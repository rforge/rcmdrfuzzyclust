#' MANOVA analysis of cluster
#' @description MANOVA analysis based on Pillai Statistic
#' @param clust cluster object
#' @return statistic of MANOVA
#' @importFrom stats manova
#' @export
#' @examples
#' library(RcmdrPlugin.FuzzyClust)
#' fuzzy.CM(X=iris[,1:4],K = 3,m = 2,RandomNumber = 1234)->cl
#' checkManova(cl)

checkManova<-function(clust){
  p<-ncol(clust$Clust.desc)
  data<-clust$Clust.desc[,1:p-1]
  cluster<-clust$Clust.desc[,p]
  Cluster<-factor(cluster)
  fit<-manova(data~factor(Cluster))
  manov<-summary(fit,test= c("Pillai"))
  return(manov$stats)
  }
