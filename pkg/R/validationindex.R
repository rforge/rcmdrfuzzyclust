#' Validation Index of Fuzzy Clustering
#'
#' @param cluster Cluster Result from Fuzzy Clustering
#'
#' @return XB.index Xie Beni index
#' @return MPC.index Modified Partition Coeeficient
#' @return CE.index Classification Entropy
#'
#' @details This function provide validation index that calculated from fuzzy clustering
#' result. There are 3 index that calculated, Xie Beni, MPC, and CE index. Both three indexes
#' calculated from fuzzy membership and data point.
#' @details Xie Beni index calculated compactness and separation of clustering.
#' @details The best cluster result can be decided with minimum value of index.
#' @references Wang, W., & Zhang, Y. (2007). On Fuzzy Cluster Validity Indices. Fuzzy Sets and System, 2095-2117.
#' @export
#'
#' @examples
#' library(RcmdrPlugin.FuzzyClust)
#' fuzzy.CM(X=iris[,1:4],K = 3,m = 2,RandomNumber = 1234)->cl
#' validation.index(cl)
validation.index<-function(cluster){
  n<-nrow(cluster$Clust.desc)
  p<-ncol(cluster$Clust.desc)
  data.X<-cluster$Clust.desc[,1:p-1]
  U<-cluster$U
  V<-cluster$V
  m<-cluster$m
  D<-cluster$D
  K<-nrow(cluster$V)
  #Partition Coefficient
  PC.index=sum((U)^2)/n
  #Modified Partition Coefficient
  MPC.index=(1-(K/(K-1))*(1-PC.index))

  #Partition Entropy
  CE.index<-10^10
  try(CE.index<-sum(U*log(U,base=exp(1)))/n*-1,silent=T)

  #Xie Beni Index
  XB.index<-10^10
  try({
    XB.temp1<-matrix(0,n,K)
    for(i in 1:n)
      for(k in 1:K)
        XB.temp1[i,k]<-D[i,k]*(U[i,k]^m)

      XB.temp2<-matrix(0,K,K)
      for(k1 in 1:K)
        for(k2 in 1:K)
          XB.temp2[k1,k2]<-t(V[k1,]-V[k2,])%*%(V[k1,]-V[k2,])
      XB.min<-min(XB.temp2[lower.tri(XB.temp2)])
      XB.index<-sum(XB.temp1)/(XB.min*n)
  },silent=T)

  #S Index
  S.index<-10^10
  try({
    S.temp1<-matrix(0,n,K)
    for(i in 1:n)
      for(k in 1:K)
        S.temp1[i,k]<-D[i,k]*(U[i,k]^2)

      S.temp2<-matrix(0,K,K)
      for(k1 in 1:K)
        for(k2 in 1:K)
          S.temp2[k1,k2]<-t(V[k1,]-V[k2,])%*%(V[k1,]-V[k2,])
      S.min<-min(S.temp2[lower.tri(S.temp2)])
      S.index<-sum(S.temp1)/(S.min*n)
  },silent=T)

  validation<-c(MPC.index,CE.index,XB.index,S.index)
  class(validation)<-"validation"
  validation
  return(validation)
}

#' @export
print.validation<-function(x,...){
  cat("Validation Index")
  cat("\nMPC Index\t:",x[1])
  cat("\nCE Index\t:",x[2])
  cat("\nXB Index\t:",x[3])
  cat("\nS Index \t:",x[4])
}

