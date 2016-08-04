#' Soft Voting Cluster Ensemble
#'
#' @description This function used to perform Soft Voting Cluster Ensemble
#'
#' @param data data frame nxp
#' @param K specific number of cluster (must be >1)
#' @param m fuzzifier / degree of fuzziness
#' @param max.iteration maximum iteration to convergence
#' @param threshold threshold of convergence
#' @param seed number of ensemble
#' @param gamma parameter of Gustafson Kessel Clustering
#' @param method fuzzy clustering method that will be used ("FCM" or "GK")
#' @param rho parameter of volume clustering in Gustafson Kessel Clustering
#'
#' @return func.obj objective function that calculated.
#' @return U matrix n x K consist fuzzy membership matrix
#' @return V matrix K x p consist fuzzy centroid
#' @return D matrix n x K consist distance of data to centroid that calculated
#' @return Clust.desc cluster description (dataset with additional column of cluster label)
#' @return seeding list of random number that used as seeding
#' @return Call call argument
#'
#' @details This function perform Soft Voting Cluster Ensemble algorithm
#'
#' @export
#' @import clue
#' @import foreach
#' @import MASS
#' @import doParallel
#' @import iterators
#' @import parallel
soft.vote.ensemble<-function(data,
                             seed,
                             method="FCM",
                             K=2,
                             m=2,
                             gamma=0,
                             rho=rep(1,K),
                             threshold=10^-5,
                             max.iteration=100)
{
  numb.seed<-seq(1:seed)
  seeding<-sample(seq(1,100),seed)
  fuzzy.CM.parallel<-function(X,K,m,RandomNumber){
    fuzzy.CM(X,K,m,RandomNumber = RandomNumber,threshold = threshold,max.iteration=max.iteration)->clus
    return(list(clus$U,clus$Clust.desc[,ncol(clus$Clust.desc)]))
  }
  fuzzy.GK.parallel<-function(X,K,m,RandomNumber,gamma){
    fuzzy.GK(X,K,m,RandomNumber = RandomNumber,gamma=gamma,threshold = threshold,max.iteration=max.iteration)->clus
    return(list(clus$U,clus$Clust.desc[,ncol(clus$Clust.desc)]))
  }

  cl<-makeCluster(detectCores()-1)
  registerDoParallel(cl)
  if(method=="FCM"){
    system.time(
      clu.par <-
        foreach(s=seeding, .combine='rbind') %dopar%{
          fuzzy.CM.parallel(data,K,m,s)
        })
  }else
  {system.time(
    clu.par <-
      foreach(s=seeding, .combine='rbind') %dopar%{
        fuzzy.GK.parallel(data,K,m,s,gamma)
      })
  }
  rownames(clu.par)<-NULL
  minWeightBipartiteMatching <- function(clusteringA, clusteringB) {
    nA <- nrow(clusteringA)  # number of instances in a
    nB <- nrow(clusteringB)  # number of instances in b
    if ( nA != nB) {
      stop("number of cluster or number of instances do not match")
    }
    assignmentMatrix<-10000-(t(clusteringA)%*%clusteringB)

    # optimization
    result <- solve_LSAP(assignmentMatrix, maximum = FALSE)
    attr(result, "assignmentMatrix") <- assignmentMatrix
    return(result)
  }
  standar<-clu.par[1,1][[1]]
  for(i in numb.seed[-1])
  {
    minWeightBipartiteMatching(clu.par[i,1][[1]],standar)->matching
    clu.par[i,2][[1]]->clusterA
    tmp <- sapply(1:length(matching), function(i) {
      clusterA[which(clu.par[i,2][[1]] == i)] <<- matching[i]
    })
    U.temp<-U.temp2<-clu.par[i,1][[1]]
    for(j in 1:length(tmp)){
      U.temp[,j]=U.temp2[,tmp[j]]
    }
    clu.par[i,1][[1]]<-U.temp
  }
  U.ensemble<-clu.par[1,1][[1]]
  for(i in 2:seed)
    U.ensemble<-U.ensemble+clu.par[i,1][[1]]
  #edit U
  U.ensemble<-U.ensemble/rowSums(U.ensemble)
  data<-as.matrix(data)
  p<-ncol(data)
  V.ensemble <- t(U.ensemble ^ m) %*% data / colSums(U.ensemble ^ m)
  if(method=="FCM"){
    D<-matrix(0,nrow=nrow(data),ncol=K)
    for(k in 1:K)
      for (i in 1:nrow(data))
      {
        D[i,k] = t(data[i,] - V.ensemble[k,]) %*% (data[i,] -V.ensemble[k,])
      }
  }else{
    F<-array(0,c(p,p,K))
    D<-matrix(0,nrow=nrow(data),ncol=K)
    for(k in 1:K){
      F[,,k] = as.matrix(0,p,p)
      F.bantu <- F[,,k]
      for (i in 1:nrow(data))
      {
        F.bantu = (U.ensemble[i,k] ^ m) * (data[i,] - V.ensemble[k,]) %*%
          t((data[i,] - V.ensemble[k,]))+F.bantu
      }
      F.bantu = F.bantu / sum(U.ensemble[,k] ^ m)
      F.bantu = (1 - gamma) * F.bantu + (gamma * (det(cov(data))) ^ (1 / p)) * diag(p)
      if (kappa(F.bantu) > 10 ^ 15)
      {
        eig <- eigen(F.bantu)
        eig.values <- eig$values
        eig.vec <- eig$vectors
        eig.val.max <- max(eig.values)
        eig.values[eig.values*(10^15)<eig.val.max]=eig.val.max/(10^15)
        F.bantu = eig.vec %*% diag(eig.values) %*% ginv(eig.vec)
      }
      detMat= det(F.bantu)
      for (i in 1:nrow(data))
      {
        D[i,k] = t(data[i,] - V.ensemble[k,]) %*% (
          (rho[k] * (detMat ^ (1 / p)))*ginv(F.bantu)) %*%
          (data[i,] -V.ensemble[k,])
      }}
  }
  Clust.desc <- matrix(0,nrow(data),p + 1)
  rownames(Clust.desc) <- rownames(data)
  colnames(Clust.desc) <- c(colnames(data),"cluster")
  Clust.desc[,1:p] <- data
  for (i in 1:nrow(data))
    Clust.desc[i,p + 1] <- which.max(U.ensemble[i,])

  colnames(V.ensemble)<-colnames(data)
  colnames(U.ensemble) = paste("Clust",1:K,sep = " ")
  func.obj = sum(U.ensemble ^ m * D)
  stopCluster(cl)
  stopImplicitCluster()
  result<-list()
  result$U<-U.ensemble
  result$V<-V.ensemble
  result$func.obj<-func.obj
  result$D<-D
  result$m<-m
  result$Clust.desc<-Clust.desc
  result$call<-match.call()
  result$seeding<-seeding
  class(result)<-"fuzzyclust"
  print(result)
  return(result)
}
