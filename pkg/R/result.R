#' Result GUI
#' @description Result GUI
#' @param parent parent window
#' @param cluster cluster object
#' @param valid validation index object
#' @param manov manova object
#' @param method method of clustering
#' @export
#' @import ggplot2
#' @import reshape2
#' @import tcltk2
#' @import tkrplot
#' @import Rcmdr
result.GUI <- function(parent,cluster,valid,manov,method) {
  result <- tktoplevel(background = "white")
  tktitle(result) <- "Result"
  fontTitle <-
    tkfont.create(
      family = "Gentium Book Basic", size = 13,
      weight = "bold"
    )
  fontCommand <- tkfont.create(family = "Gentium Basic", size = 10,
                               weight = "bold")
  fontLabel <- tkfont.create(
    family = "Gentium Basic", size = 10,
    weight = "bold",slant = "italic"
  )
  resultlabel <-
    paste("Fuzzy Clustering Analysis\nMethod:",method)
  tkgrid(
    tk2label(
      result,text = resultlabel,font =
        fontTitle
    ),row = 0,column = 0,pady = c(5,5),padx = 5,sticky="ew",columnspan=2
  )
  result$option<-tk2frame(result,borderwidth =1, relief="flat")
  result$output<-tk2frame(result,borderwidth=2,relief="solid",width=625,height=475)
  tkgrid(result$option,
         pady=5,padx=5,sticky="nw")
  tkgrid.configure(result$output,column=1,row=1,padx=5,pady=5)
  tkgrid.propagate(result$output,F)

  option.list <- tk2listbox(result$option, height = 8,width=30,
                            selectmode = "single",relief="solid",scroll="none")
  tkgrid(tk2label(result$option,
                  text = "Output Choices:", justify = "left",font=fontLabel),
         padx = 5, pady =c(5, 5), sticky = "w")
  result$output$panel<-tk2panedwindow(result$output,orientation = "vertical")
  tkgrid(option.list, padx = 5, pady = c(5, 5),sticky="w")

  options <- c("Biplot Cluster", "Fuzzy Membership",
               "Cluster Centroid", "Validation Index & MANOVA")
  for (o in options)
    tkinsert(option.list, "end", o)
  tkselection.set(option.list, 0)

  #----------------------------------------------------#
  #  Output. 1 Plot Biplot Cluster and Labeling        #
  #----------------------------------------------------#

  biplot.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")
  plot.label <-
    tk2label(
      biplot.panel,text = "Biplot Cluster",justify =
        "left",font = fontLabel
    )
  ####-> Prepare PCA Data and Plotting
  pp <- ncol(cluster$Clust.desc)
  data.clu <- cluster$Clust.desc[,1:pp - 1]
  data.PCA <- prcomp(data.clu,scale. = T)
  z1 <- as.data.frame(cbind(data.PCA$x[,1:2],cluster$Clust.desc[,pp]))

  windowsFonts(A=windowsFont("Gentium Basic"))
  ploting <- function() {
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
  }
  tkrplotClus<-function(parent,fun){
    image <- paste("Rplot", .make.tkindex(), sep = "")
    win.metafile(width = 3, height = 3, restoreConsole = FALSE)
    .my.tkdev(1.3, 1.3)
    try(fun())
    .Tcl(paste("image create Rplot", image))
    lab <- tk2label(parent, image = image)
    tkpack.propagate(lab,F)
    tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete",
                                                   image)))
    lab$image <- image
    lab$fun <- fun
    lab
  }
  clusplot <- tkrplotClus(
    biplot.panel, fun = ploting)
  copyClusplot<-function(){
    image <- paste("Rplot", .make.tkindex(), sep = "")
    win.metafile(width = 3, height = 3, restoreConsole = FALSE)
    .my.tkdev(1.5, 1.5)
    try(ploting())
    .Tcl(paste("image create Rplot",image))
  }
  copClus<-tk2button(result$option,text="Copy Cluster Plot",width=25,command=copyClusplot)
  tkgrid(copClus,padx=5,pady=c(5,5),sticky="w")

  tkgrid(
    plot.label,row = 0,column = 0,padx = 2,sticky="nw"
  )
  tkgrid(
   clusplot,
    row = 1,column = 0,sticky = "nw",padx = 5,pady =
      c(0,0)
  )

  #----------------------------------------------------#
  #  Output. 2 Fuzzy Membership Matrix                 #
  #----------------------------------------------------#
  fuzzy.member.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")
  fuzzy.member.label <-
    tk2label(
      fuzzy.member.panel,text = "Fuzzy Membership Matrix",justify =
        "left",font = fontLabel
    )
  tkgrid(fuzzy.member.label,sticky="w",padx = 5)
  tclTableU <- tclArray()

  mat.U <- rbind(paste("Cluster",1:ncol(cluster$U)),cluster$U)
  mat.U <- cbind(c("Observation",rownames(cluster$Clust.desc)),c("Label",cluster$Clust.desc[,pp]),mat.U)
  for (i in 1:nrow(mat.U))
    for (j in 1:ncol(mat.U)){
      if (i ==1 ||j==1)
        tclTableU[[i - 1, j - 1]] <- strsplit(mat.U[i, j]," ",fixed = T)[[1]]
      else
        tclTableU[[i - 1, j - 1]] <- round(as.numeric(mat.U[i, j]),5)}
  U.table <-
    tk2table(
      fuzzy.member.panel,
      variable = tclTableU,titlerows = 1,titlecols = 1,
      width =4,height = 20,
      rows = nrow(mat.U),
      cols = ncol(mat.U),
      selectmode = "extended", colwidth = 20,
      background = "white",yscrollcommand = function(...)
        tkset(yscr,...),xscrollcommand = function(...)
          tkset(xscr, ...)
    )
  yscr <- tk2scrollbar(
    fuzzy.member.panel, orient = "vertical",
    command = function(...)
      tkyview(U.table, ...)
  )
  xscr <- tk2scrollbar(
    fuzzy.member.panel, orient = "horizontal",
    command = function(...)
      tkxview(U.table, ...)
  )
  tkgrid(
    U.table,yscr,sticky = "w",padx = 5,pady =
      c(5,5)
  )
  tkconfigure(U.table,state = "disable")
  tkconfigure(U.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")
  tkgrid.configure(yscr, sticky = "nsw")
  tkgrid(xscr, sticky = "new")
  tkgrid.rowconfigure(U.table, 0, weight = 1)
  tkgrid.columnconfigure(U.table, 0, weight = 1)
  tkgrid.columnconfigure(U.table, 1, weight = 1)

  #----------------------------------------------------#
  #  Output. 3 Centroid Cluster                        #
  #----------------------------------------------------#
  fuzzy.centroid.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")
  fuzzy.centroid.label <-
    tk2label(
      fuzzy.centroid.panel,text = "Fuzzy Cluster Centroid",justify =
        "left",font = fontLabel
    )
  tkgrid(fuzzy.centroid.label,sticky="w")
  tclTableV <- tclArray()

  mat.V <- rbind(paste(colnames(cluster$V)),cluster$V)
  mat.V <- cbind(c("",paste("Cluster",1:nrow(cluster$V))),mat.V)
  mat.V <- t(mat.V)
  for (i in 1:nrow(mat.V))
    for (j in 1:ncol(mat.V))
    {
      if (i ==1 || j==1)
        tclTableV[[i - 1, j - 1]] <- strsplit(mat.V[i, j]," ",fixed = T)[[1]]
      else
        tclTableV[[i - 1, j - 1]] <- round(as.numeric(mat.V[i, j]),5)
    }
  V.table <-
    tk2table(
      fuzzy.centroid.panel, variable = tclTableV,
      titlerows = 1,titlecols = 1,
      width =3,
      height = 4,
      rows = nrow(mat.V),
      cols = ncol(mat.V),
      selectmode = "extended",
      colwidth = 25,
      background = "white",yscrollcommand = function(...)
        tkset(Vyscr,...),xscrollcommand = function(...)
          tkset(Vxscr, ...)
    )
  Vyscr <- tk2scrollbar(
    fuzzy.centroid.panel, orient = "vertical",
    command = function(...)
      tkyview(V.table, ...)
  )
  Vxscr <- tk2scrollbar(
    fuzzy.centroid.panel, orient = "horizontal",
    command = function(...)
      tkxview(V.table, ...)
  )
  tkconfigure(V.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")
  radar.plotting<-function(){
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
      xlab("") + ylab("") + geom_text(data=dataLab,aes(x=V1,y=V2,label=V2))+
      guides(color = guide_legend(ncol=1)) +
      coord_radar()->radar
    eval(substitute(print(radar)))
  }
  tkrplotRadar<-function(parent,fun){
    image <- paste("Rplot", .make.tkindex(), sep = "")
    win.metafile(width = 3, height = 3, restoreConsole = FALSE)
    .my.tkdev(1, 1)
    try(fun())
    .Tcl(paste("image create Rplot", image))
    lab <- tk2label(parent, image = image)
    tkpack.propagate(lab,F)
    tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete",
                                                   image)))
    lab$image <- image
    lab$fun <- fun
    lab
  }
  radarplot <- tkrplotRadar(
    fuzzy.centroid.panel, fun = radar.plotting)
  copyRadplot<-function(){
    image <- paste("Rplot", .make.tkindex(), sep = "")
    win.metafile(width = 3, height = 3, restoreConsole = FALSE)
    .my.tkdev(1.5, 1.5)
    try(radar.plotting())
    .Tcl(paste("image create Rplot",image))
  }
  copRad<-tk2button(result$option,text="Copy Radar Plot",width=25,command=copyRadplot)
  tkgrid(copRad,padx=5,pady=c(5,5),sticky="w")
  tkgrid(radarplot,pady = c(0,0),padx = 5,columnspan=2)
  tkgrid(
    V.table,Vyscr,sticky = "w",padx = 5,pady =
      c(0,0)
  )
  tkgrid.configure(Vyscr, sticky = "nsw")
  tkgrid(Vxscr, sticky = "new")
  tkgrid.rowconfigure(V.table, 0, weight = 1)
  tkgrid.columnconfigure(V.table, 0, weight = 1)
  tkconfigure(V.table,state = "disable")
  #----------------------------------------------------#
  #  Output. 4 Validation and Manova                   #
  #----------------------------------------------------#
  validation.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")

  tkgrid(
    tk2label(validation.panel,text = "Validation Index",
             justify = "left",font =
               fontLabel),
    padx = 5,sticky = "w"
  )
  tclValid <- tclArray()

  mat.val <- c("Index Value",valid[1],valid[2],valid[3])
  mat.val <- as.matrix(mat.val)
  mat.val <-
    cbind(c("Index Name","MPC Index","CE Index","Kwon index"),mat.val)
  for (i in 1:nrow(mat.val))
    for (j in 1:ncol(mat.val)){
      if (i < 2)
        tclValid[[i - 1, j - 1]] <- strsplit(mat.val[i, j]," ",fixed = T)[[1]]
      else if (j < 2)
        tclValid[[i - 1, j - 1]] <- strsplit(mat.val[i, j]," ",fixed = T)[[1]]
      else
        tclValid[[i - 1, j - 1]] <- round(as.numeric(mat.val[i, j]),5)}
  Valid.table <-
    tk2table(
      validation.panel, variable = tclValid,
      titlerows = 1,titlecols = 1, width =
        2,height = 4,rows = nrow(mat.val),cols = ncol(mat.val),selectmode = "extended", colwidth = 15, background = "white"
    )
  tkgrid(
    Valid.table,sticky = "w"
  )
  tkgrid.rowconfigure(Valid.table, 0, weight = 1)
  tkgrid.columnconfigure(Valid.table, 0, weight = 1)
  tkconfigure(Valid.table,state = "disable")
  tkconfigure(Valid.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")

  ####- Manova
  tkgrid(
    tk2label(validation.panel,text = "MANOVA Analysis",justify = "left",font =
               fontLabel),pady = c(5,5),padx = 5,sticky = "w"
  )
  tclManov <- tclArray()

  mat.man <- rbind(colnames(manov),manov)
  mat.man <- cbind(c("",row.names(manov)),mat.man)

  for (i in 1:nrow(mat.man))
    for (j in 1:ncol(mat.man))
    {    if (i < 2)
      tclManov[[i - 1, j - 1]] <- strsplit(mat.man[i, j]," ",fixed = T)[[1]]
    else if (j < 2)
      tclManov[[i - 1, j - 1]] <- strsplit(mat.man[i, j]," ",fixed = T)[[1]]
    else if (is.na(mat.man[i,j]))
      tclManov[[i - 1,j - 1]] <- strsplit(mat.man[i, j]," ",fixed = T)[[1]]
    else
      tclManov[[i - 1, j - 1]] <- round(as.numeric(mat.man[i, j]),3)}

  man.table <-
    tk2table(
      validation.panel, variable = tclManov,titlerows = 1,titlecols = 1,
      height =
        3,
      rows = nrow(mat.man),
      cols = ncol(mat.man),selectmode = "extended",
      colwidth = 10, background = "white"
    )
  tkgrid(man.table,sticky = "w",padx = 5)
  tkgrid.rowconfigure(man.table, 0, weight = 1)
  tkgrid.columnconfigure(man.table, 0, weight = 1)
  tkconfigure(man.table,state = "disable")
  tkconfigure(man.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")

  ###################################
  onClick <- function() {
    as.numeric(tkcurselection(option.list))->choice
    print(choice)
    tcl(result$output$panel,"forget",0)
    if(choice==0) {
      tkadd(result$output$panel,biplot.panel)
    }
    else if(choice==1){
      tkadd(result$output$panel,fuzzy.member.panel)
    }
    else if(choice==2){
      tkadd(result$output$panel,fuzzy.centroid.panel)
    }
    else{
      tkadd(result$output$panel,validation.panel)
    }
  }
  tkadd(result$output$panel,biplot.panel)
  tkgrid(result$output$panel,sticky = "nw",padx=5,pady=5)

  tkbind(option.list,"<Double-1>",onClick)
  tkfocus(result)
  tkdestroy(parent)
}
