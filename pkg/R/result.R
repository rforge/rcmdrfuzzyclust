#' Result GUI
#' @description Result GUI
#' @details Not run by users
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
#' @importFrom grDevices dev.off png dev.new
result.GUI <- function(parent,cluster,valid,manov,method) {
  result <- tcltk::tktoplevel(background = "white")
  tcltk::tktitle(result) <- "Result"
  fontTitle <-
    tcltk::tkfont.create(
      family = "Gentium Book Basic", size = 13,
      weight = "bold"
    )
  fontCommand <- tcltk::tkfont.create(family = "Gentium Basic", size = 10,
                               weight = "bold")
  fontLabel <- tcltk::tkfont.create(
    family = "Gentium Basic", size = 10,
    weight = "bold",slant = "italic"
  )
  resultlabel <-
    paste("Fuzzy Clustering Analysis")
  tcltk::tkgrid(
    tk2label(
      result,text = resultlabel,font =
        fontTitle
    ),row = 0,column = 0,pady = c(5,5),padx = 5,
    columnspan=2
  )
  methodlabel<-paste("Methods: ",method,sep="")
  cat(methodlabel)
  tcltk::tkgrid(
    tk2label(result,text = methodlabel,font =
        fontTitle
    ),row = 1,column = 0,pady = c(5,5),padx = 5,
    columnspan=2
  )
  result$option<-tk2frame(result,borderwidth =1, relief="flat")
  result$output<-tk2frame(result,borderwidth=2,relief="solid",width=625,height=500)
  tcltk::tkgrid(result$option,
         pady=5,padx=5,sticky="nw")
  tcltk::tkgrid.configure(result$output,column=1,row=2,padx=5,pady=5)
  tcltk::tkgrid.propagate(result$output,F)

  option.list <- tk2listbox(result$option, height = 8,width=30,
                            selectmode = "single",relief="solid",scroll="none")
  tcltk::tkgrid(tk2label(result$option,
                  text = "Output Choices:\n(Double Click)", justify = "left",font=fontLabel),
         padx = 5, pady =c(5, 5), sticky = "w")
  result$output$panel<-tk2panedwindow(result$output,orientation = "vertical")
  tcltk::tkgrid(option.list, padx = 5, pady = c(5, 5),sticky="w")

  options <- c("Biplot Cluster", "Fuzzy Membership",
               "Cluster Centroid", "Validation Index & MANOVA")
  for (o in options)
    tcltk::tkinsert(option.list, "end", o)
  tcltk::tkselection.set(option.list, 0)
  report.button<-tk2button(result$option,
                           text="Generate Report",width=25)

  #----------------------------------------------------#
  #  Output. 1 Plot Biplot Cluster and Labeling        #
  #----------------------------------------------------#

  biplot.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")
  biplot.label.panel <-
    tk2frame(biplot.panel,borderwidth = 2,relief = "flat")
  plot.label <-
    tk2label(
      biplot.label.panel,text = "Biplot Cluster",justify =
        "left",font = fontLabel
    )
  pp <- ncol(cluster$Clust.desc)

  ####-> Prepare PCA Data and Plotting
  tkrplotClus<-function(parent,fun,param){
    image <- paste("Rplot", .make.tkindex(), sep = "")
    dev.new(width = 3, height = 3, restoreConsole = FALSE)
    .my.tkdev(1.3, 1.3)
    try(fun(param))
    .Tcl(paste("image create Rplot", image))
    lab <- tk2label(parent, image = image)
    tcltk::tkpack.propagate(lab,F)
    tcltk::tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete",
                                                   image)))
    lab$image <- image
    lab$fun <- fun
    lab$param<-param
    lab

  }
  clusplot <- tkrplotClus(
    biplot.panel, fun = biploting,param=cluster)

  tcltk::tkgrid(biplot.label.panel,sticky="w")
  tcltk::tkgrid(
    plot.label,
    row = 0,padx = 2,sticky="w"
  )
  tcltk::tkgrid(
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
  tcltk::tkgrid(fuzzy.member.label,sticky="w",padx = 5)
  tclTableU <-
    tcltk::tclArray()

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
      variable = tclTableU,
      width =4,height = 20, titlerows = 1,titlecols=1,
      rows = nrow(mat.U),
      cols = ncol(mat.U),
      selectmode = "extended", colwidth = 20,
      background = "white",yscrollcommand = function(...)
        tcltk::tkset(yscr,...),xscrollcommand = function(...)
          tcltk::tkset(xscr, ...)
    )
  yscr <- tk2scrollbar(
    fuzzy.member.panel, orientation = "vertical",
    command = function(...)
      tcltk::tkyview(U.table, ...)
  )
  xscr <- tk2scrollbar(
    fuzzy.member.panel, orientation= "horizontal",
    command = function(...)
      tcltk::tkxview(U.table, ...)
  )
  tcltk::tkgrid(
    U.table,yscr,sticky = "w",padx = 5,pady =
      c(5,5)
  )
  tcltk::tkconfigure(U.table,state = "disable")
  tcltk::tkconfigure(U.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")
  tcltk::tkgrid.configure(yscr, sticky = "nsw")
  tcltk::tkgrid(xscr, sticky = "new")
  tcltk::tkgrid.rowconfigure(U.table, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(U.table, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(U.table, 1, weight = 1)

  #----------------------------------------------------#
  #  Output. 3 Centroid Cluster                        #
  #----------------------------------------------------#
  fuzzy.centroid.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")
  fuzzy.centroid.panel.label <-
    tk2frame(fuzzy.centroid.panel,borderwidth = 2,relief = "flat")
  fuzzy.centroid.label <-
    tk2label(
      fuzzy.centroid.panel.label,text = "Fuzzy Cluster Centroid",justify =
        "left",font = fontLabel
    )
  tclTableV <- tcltk::tclArray()

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
        tcltk::tkset(Vyscr,...),xscrollcommand = function(...)
          tcltk::tkset(Vxscr, ...)
    )
  Vyscr <- tk2scrollbar(
    fuzzy.centroid.panel, orientation= "vertical",
    command = function(...)
      tcltk::tkyview(V.table, ...)
  )
  Vxscr <- tk2scrollbar(
    fuzzy.centroid.panel, orientation= "horizontal",
    command = function(...)
      tcltk::tkxview(V.table, ...)
  )
  tcltk::tkconfigure(V.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")
  tkrplotRadar<-function(parent,fun,param){
    image <- paste("Rplot", .make.tkindex(), sep = "")
    dev.new(width = 3, height = 3, restoreConsole = FALSE)
    .my.tkdev(1, 1)
    try(fun(param))
    .Tcl(paste("image create Rplot", image))
    lab <- tk2label(parent, image = image)
    tcltk::tkpack.propagate(lab,F)
    tcltk::tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete",
                                                   image)))
    lab$image <- image
    lab$fun <- fun
    lab$param<-param
    lab

  }
  radarplot <- tkrplotRadar(
    fuzzy.centroid.panel, fun = radar.plotting,param=cluster)
  tcltk::tkgrid(fuzzy.centroid.panel.label,sticky="w")
  tcltk::tkgrid(fuzzy.centroid.label,sticky="w")
  tcltk::tkgrid(radarplot,pady = c(0,0),padx = 5,columnspan=2)
  tcltk::tkgrid(
    V.table,Vyscr,sticky = "w",padx = 5,pady =
      c(0,0)
  )
  tcltk::tkgrid.configure(Vyscr, sticky = "nsw")
  tcltk::tkgrid(Vxscr, sticky = "new")
  tcltk::tkgrid.rowconfigure(V.table, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(V.table, 0, weight = 1)
  tcltk::tkconfigure(V.table,state = "disable")
  #----------------------------------------------------#
  #  Output. 4 Validation and Manova                   #
  #----------------------------------------------------#
  validation.panel <-
    tk2frame(result$output$panel,borderwidth = 2,relief = "flat")

  tcltk::tkgrid(
    tk2label(validation.panel,text = "Validation Index",
             justify = "left",font =
               fontLabel),
    padx = 5,sticky = "w"
  )
  tclValid <- tcltk::tclArray()

  mat.val <- c("Index Value",valid[1],valid[2],valid[3],valid[4])
  mat.val <- as.matrix(mat.val)
  mat.val <-
    cbind(c("Index Name","MPC Index","CE Index","XB index","S Index"),mat.val)
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
        2,height = 5,rows = nrow(mat.val),cols = ncol(mat.val),selectmode = "extended", colwidth = 15, background = "white"
    )
  tcltk::tkgrid(
    Valid.table,sticky = "w"
  )
  tcltk::tkgrid.rowconfigure(Valid.table, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(Valid.table, 0, weight = 1)
  tcltk::tkconfigure(Valid.table,state = "disable")
  tcltk::tkconfigure(Valid.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")

  ####- Manova
  tcltk::tkgrid(
    tk2label(validation.panel,text = "MANOVA Analysis",justify = "left",font =
               fontLabel),pady = c(5,5),padx = 5,sticky = "w"
  )
  tclManov <- tcltk::tclArray()

  mat.man <- rbind(colnames(manov),manov)
  mat.man <- cbind(c("","Cluster","Residuals"),mat.man)

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
  tcltk::tkgrid(man.table,sticky = "w",padx = 5)
  tcltk::tkgrid.rowconfigure(man.table, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(man.table, 0, weight = 1)
  tcltk::tkconfigure(man.table,state = "disable")
  tcltk::tkconfigure(man.table, selectmode = "extended",
              rowseparator = "\"\n\"", colseparator = "\"\t\"")

  ###################################
  onReport<-function(){
    wdfirst<-getwd()
    owd<-setwd(tempdir())
    on.exit(owd)
    cluster.fuzzy<-list(cluster,valid,manov)
    save(cluster.fuzzy,file="cluster.Rda")
    png("Biplot.png")
    biploting(cluster)
    dev.off()
    png("Radar.png")
    radar.plotting(cluster)
    dev.off()
    Try <- function(expr) {
      res <- try(expr, silent = TRUE)
      if (inherits(res, "try-error")) {
        res <- sub("^.+\n +\\[tcl\\] ", "Tcl error: ", res)
        tcltk::tkmessageBox(title = "An error has occured!",
                     message = as.character(res), icon = "error", type = "ok")
      }
      res
    }
    rmd.report.path<-path.package(package="RcmdrPlugin.FuzzyClust")
    rmd.report.path<-paste(rmd.report.path,"/Report.Rmd",sep="")
    file.copy(rmd.report.path,"Report.Rmd")
    Try(rmarkdown::render("Report.Rmd",output_file=paste(wdfirst,"/Report.docx",sep="")))
    res <- tkmessageBox(title = "",
                        message = paste("Check Report.docx in",
                                        paste(wdfirst,
                                              "/Report.docx",sep=""),
                                        "directory :)",sep=" "),
                                        icon = "info", type = "ok")
    setwd(wdfirst)
  }
  tcltk::tkconfigure(report.button,command=onReport)
  tcltk::tkgrid(report.button,pady=5,padx=5)
  onClick <- function() {
    as.numeric(tcltk::tkcurselection(option.list))->choice
    print(choice)
    tcl(result$output$panel,"forget",0)
    if(choice==0) {
      tcltk::tkadd(result$output$panel,biplot.panel)
    }
    else if(choice==1){
      tcltk::tkadd(result$output$panel,fuzzy.member.panel)
    }
    else if(choice==2){
      tcltk::tkadd(result$output$panel,fuzzy.centroid.panel)
    }
    else{
      tcltk::tkadd(result$output$panel,validation.panel)
    }
  }
  tcltk::tkadd(result$output$panel,biplot.panel)
  tcltk::tkgrid(result$output$panel,sticky = "nw",padx=5,pady=5)

  tcltk::tkbind(option.list,"<Double-1>",onClick)
  tcltk::tkfocus(result)
  tcltk::tkdestroy(parent)
}
