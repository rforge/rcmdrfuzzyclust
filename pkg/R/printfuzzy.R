#' @export
print.fuzzyclust<-function(x,..){
  cat("Call:\n")
  print(x$call)
  cat("\nObjective Function:",x$func.obj)
  cat("\nfuzzifier:",x$m)
  cat("\nCentroid:\n")
  print(x$V)
  cat("\nCluster Label:\n")
  print(x$Clust.desc[,ncol(x$Clust.desc)])
}
