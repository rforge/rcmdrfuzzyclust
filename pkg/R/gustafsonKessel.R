#' Gustafson Kessel Improved Covariance Estimation
#'
#' @description This function used to perform Gustafson Kessel Clustering of X dataset.
#'
#' @param X data frame n x p
#' @param K specific number of cluster (must be >1)
#' @param m fuzzifier / degree of fuzziness
#' @param max.iteration maximum iteration to convergence
#' @param threshold threshold of convergence
#' @param RandomNumber specific seed
#' @param Gamma tuning parameter of covariance
#'
#' @return func.obj objective function that calculated.
#' @return U matrix n x K consist fuzzy membership matrix
#' @return V matrix K x p consist fuzzy centroid
#' @return D matrix n x K consist distance of data to centroid that calculated
#' @return Clust.desc cluster description (dataset with additional column of cluster label)
#'
#'
#' @details This function perform Fuzzy C-Means algorithm by Gustafson Kessel (1968) that improved by Babuska et al (2002).
#' Gustafson Kessel (GK) is one of fuzzy clustering methods to clustering dataset
#' become K cluster. Number of cluster (K) must be greater than 1. To control the overlaping
#' or fuzziness of clustering, parameter m must be specified.
#' Maximum iteration and threshold is specific number for convergencing the cluster.
#' Random Number is number that will be used for seeding to firstly generate fuzzy membership matrix.
#' @details Clustering will produce fuzzy membership matrix (U) and fuzzy cluster centroid (V).
#' The greatest value of membership on data point will determine cluster label.
#' Centroid or cluster center can be use to interpret the cluster. Both membership and centroid produced by
#' calculating mathematical distance. Fuzzy C-Means calculate distance with Covariance Cluster norm distance. So it can be said that cluster
#' will have both sperichal and elipsodial shape of geometry.
#' @details Babuska improve the covariance estimation via tuning covariance cluster
#' with covariance of data. Tuning parameter determine proportion of covariance data and covariance cluster
#' that will be used to estimate new covariance cluster. Beside improving via tuning, Basbuka improve
#' the algorithm with decomposition of covariance so it will become non singular matrix.
#'
#' @export
fuzzy.GK<-function(X,K=2,m=1.5,max.iteration=100,
                         threshold=10^-5,RandomNumber=0,rho=rep(1,K),
                         gamma=0) {
  library(MASS)
  data.X <- as.matrix(X)
  n <- nrow(data.X)
  p <- ncol(data.X)
  ##Initiation Parameter##
  if (
    (K <= 1) || !(is.numeric(K)) || (K %% ceiling(K) > 0))
    K = 2
  if ( (m <= 1) || !(is.numeric(m)))
    m = 2
  if (RandomNumber > 0)
    set.seed(RandomNumber)
  if(length(rho)!=K)
    rho = rep(1,K)
  if(gamma<0||gamma>1)
    gamma=0

  ## Membership Matrix U (n x K)
  U <- matrix(runif(n * K,0,1),n,K)

  ## Prerequirement of U:
  ## Sum of membership on datum is 1
  U <- U / rowSums(U)

  ## Centroid Matrix V (K x p)
  V <- matrix(0,K,p)

  ## Covariance Cluster
  F <- array(0,c(p,p,K))

  ## Distance Matrix
  D <- matrix(0,n,K)

  U.old <- U + 1
  iteration = 0
  while ((max(abs(U.old - U)) > threshold) &&
         (iteration < max.iteration))
  {
    U.old <- U
    V.old<-V
    D.old<-D
    ## Calculate Centroid
    V <- t(U ^ m) %*% data.X / colSums(U ^ m)
    for (k in 1:K)
    {
      F[,,k] = as.matrix(0,p,p)
      F.bantu <- F[,,k]
      for (i in 1:n)
      {
        F.bantu = (U[i,k] ^ m) * (data.X[i,] - V[k,]) %*%
          t((data.X[i,] - V[k,]))+F.bantu
      }
      F.bantu = F.bantu / sum(U[,k] ^ m)
      F.bantu = (1 - gamma) * F.bantu + (gamma * (det(cov(data.X))) ^ (1 / p)) * diag(p)
      if (kappa(F.bantu) > 10 ^ 15)
      {
        eig <- eigen(F.bantu)
        eig.values <- eig$values
        eig.vec <- eig$vectors
        eig.val.max <- max(eig.values)
        eig.values[eig.values*(10^15)<eig.val.max]=eig.val.max/(10^15)
        F.bantu = eig.vec %*% diag(eig.values) %*% solve(eig.vec)
      }
      detMat= det(F.bantu)
      #Distance calculation
      for (i in 1:n)
      {
        D[i,k] = t(data.X[i,] - V[k,]) %*% (
          (rho[k] * (detMat ^ (1 / p)))*ginv(F.bantu,tol=0)) %*%
          (data.X[i,] -V[k,])
      }
    }
    ##FUZZY PARTITION MATRIX
    for (i in 1:n)
    {
      U[i,] <- 1 /
        (((D[i,]) ^ (1 / (m - 1))) *
           sum((1 / (D[i,])) ^ (1 /(m - 1))))
    }
    if(any(is.na(U))==T||any(is.infinite(U))==T)
    {
      U<-U.old
      V<-V.old
      D<-D.old
    }
    for (i in 1:n)
      for (k in 1:K) {
        if (U[i,k] < 0)
          U[i,k] = 0
        else if (U[i,k] > 1)
          U[i,k] = 1
      }
    func.obj = 0
    func.obj = sum(U ^ m * D)
    iteration = iteration + 1
  }
  func.obj -> func.Obj.opt
  U -> U.opt
  V -> V.opt
  D -> D.opt
  for (k in 1:K)
  {
    F[,,k] = as.matrix(0,p,p)
    F.bantu <- F[,,k]
    for (i in 1:n)
    {
      F.bantu = (U[i,k] ^ m) * (data.X[i,] - V[k,]) %*%
        t((data.X[i,] - V[k,]))+F.bantu
    }
    F.bantu = F.bantu / sum(U[,k] ^ m)}
  F->F.opt
  ###Labelling###
  colnames(V.opt)<-colnames(X)
  colnames(U.opt) = paste("Clust",1:K,sep = " ")
  Clust.desc <- matrix(0,n,p + 1)
  rownames(Clust.desc) <- rownames(X)
  colnames(Clust.desc) <- c(colnames(X),"cluster")
  Clust.desc[,1:p] <- data.X
  for (i in 1:n)
    Clust.desc[i,p + 1] <- which.max(U.opt[i,])
  result <- list()
  result$func.obj <- func.Obj.opt
  result$U <- U.opt
  result$V <- V.opt
  result$D <- D.opt
  result$m <- m
  result$call<-match.call()
  result$Clust.desc <- Clust.desc
  class(result)<-"fuzzyclust"
  result
  return(result)
}
