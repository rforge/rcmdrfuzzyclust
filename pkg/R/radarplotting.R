#' Radar Ploting Cluster Result
#' @details Make Visualization Radar Ploting from
#' @param cluster a cluster object
#' @return radarplot a radarplot
#' @import ggplot2
#' @import reshape2
#' @export
radar.plotting<-function(cluster){
  pp <- ncol(cluster$Clust.desc)
  x.mean<- apply(cluster$Clust.desc[,1:(pp-1)],2,mean)
  x.sd<-apply(cluster$Clust.desc[,1:(pp-1)],2,sd)
  as.data.frame(cluster$V)->Centro
  paste("Cluster ",1:nrow(Centro))->rownames(Centro)
  Centro<-as.data.frame(t(apply(Centro,1,function(x){(x-x.mean)/(x.sd)})))
  Centro$Cluster<-paste("Cluster ",1:nrow(Centro))
  Centromelted<-reshape2::melt(Centro)
  coord_radar <- function (theta = "x", start = 0, direction = 1)
  {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x")
      "y"
    else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
            direction = sign(direction),
            is_linear = function(coord) TRUE)
  }
  dataLab<-as.data.frame(cbind(rep(0,5),c(-1,-.5,0,.5,1)))
  ggplot(Centromelted, aes(x = variable, y = value)) +
    geom_polygon(aes(group = Cluster, color = Cluster), fill = NA, size = 1, show.legend = F) +
    geom_line(aes(group = Cluster, color = Cluster), size = 1) +
    theme(strip.text.x = element_text(size = 8),
          axis.text.x = element_text(size =8),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major=element_line(color="grey",size = .5),
          panel.background=element_rect(fill="white")) +
    xlab("") + ylab("") +
    geom_text(data=dataLab,aes(x=V1,y=V2,label=V2))+
    guides(color = guide_legend(ncol=1)) +
    coord_radar()->radar
  eval(substitute(print(radar)))
  return(radar)
}
