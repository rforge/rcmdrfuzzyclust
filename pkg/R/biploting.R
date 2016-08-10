#' Biploting Cluster Result
#' @details Make Visualization Biplot from cluster analysis result
#' @param cluster a cluster object
#' @return biplot a biplot
#' @import ggplot2
#' @export
biploting <- function(cluster) {
  pp <- ncol(cluster$Clust.desc)
  data.clu <- cluster$Clust.desc[,1:pp - 1]
  data.PCA <- prcomp(data.clu,scale. = T)
  z1 <- as.data.frame(cbind(data.PCA$x[,1:2],cluster$Clust.desc[,pp]))

  grDevices::windowsFonts(A=windowsFont("Gentium Basic"))

  datapc <- data.frame(varnames=rownames(data.PCA$rotation),
                       data.PCA$rotation)
  mult <- min(
    (max(z1[,"PC1"]) - min(z1[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"]))),
    (max(z1[,"PC2"]) - min(z1[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"])))
  )

  datapc <- transform(datapc,
                      v1 = .7 * mult * (get("PC1")),
                      v2 = .7 * mult * (get("PC2"))
  )

  ggplot(z1,
         aes(x = PC1,y = PC2,color=factor(V3))) +
    geom_point() +
    labs(color="Cluster")+
    xlab(paste("PC 1 \nVariance Explained: ",
               round(summary(data.PCA)$importance[2,1] *100,2),"%")) +
    ylab(paste("PC 2 \nVariance Explained: ",
               round(summary(data.PCA)$importance[2,2] *100,2),"%"))+
    theme_bw(base_size = 10,base_family = "A")+
    coord_equal(ratio = 1)+
    geom_text(data=datapc, aes(x=v1, y=v2, label=varnames),
              size = 3, vjust=1, color="navy",check_overlap = F)+
    geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),
                 arrow=arrow(length=unit(0.3,"cm")), color="navy")-> pl
  pl<-pl+
    geom_text(data=z1,aes(x=PC1,y=PC2,color=factor(V3),label=rownames(z1)),
              check_overlap = F,size=3)
  eval(substitute(print(pl)))
  return(pl)
}
