#' Input Plugin of Fuzzy Clustering on Rcmdr
#'
#' @description Graphical User Interface on Rcmdr Plugin.
#' This Plugin provide Interface to select variables of dataset that will be
#' used for Fuzzy Clustering, methods selection, and parameter specification
#' @export

pluginInput<-function(){
  require(tcltk2)
  require(tcltk)
  require(Rcmdr)
  #--------------------------#
  #  DATASET                 #
  #--------------------------#

  dataset <- ActiveDataSet()
  doItAndPrint(paste("data<-data.frame(",dataset,")"))
  var <- colnames(data)
  justDoIt(paste("var.choice<-c()"))
  justDoIt(paste("data.cluster<<-matrix(nrow=nrow(data))"))
  justDoIt(paste("cluster<<-list()"))
  justDoIt(paste("manov<<-list()"))
  justDoIt(paste("valid<<-list()"))

  #--------------------------#
  #  INTERFACE               #
  #--------------------------#
  #--WINDOWS-----------------#
  win <-tktoplevel(background="white")

  win1 <- tk2frame(win,relief="flat",width=470,height=365)
  tkgrid(win1)
  tkgrid.propagate(win1,F)
  fontTitle<- tkfont.create(family = "Gentium Book Basic", size = 13,
                            weight = "bold", underline = T)
  fontCommands<- tkfont.create(family = "Gentium Basic", size = 10,
                               weight="bold")

  tktitle(win) <- "Fuzzy Clustering"
  #--FRAME DATA OPTIONS------#
  tkgrid(
    tk2label(
      win1, text = "Fuzzy Clustering", font=fontTitle
    ),
    padx = 15, pady = c(5, 5),row=0,column=0
  )

  win1$frame1<-tk2frame(win1,borderwidth=2,relief="flat")
  tkgrid(win1$frame1,padx=0,pady=c(0,0),row=1,column=0,sticky="w")
  win1$var$list <-
    tk2listbox(
      win1$frame1,width = 25,height = 6,selectmode = "multiple"
    )
  tkgrid(
    tk2label(
      win1$frame1, text = "Select Variables:\n(Min. 2 Variables)", justify = "left",
      font=fontCommands
    ),
    padx = 15, pady = c(5, 5),sticky="w",row=0,column=0
  )
  tkgrid(win1$var$list, padx = 15, pady = c(0, 15),row=1,column=0,rowspan=2)
  for (i in var)
    tkinsert(win1$var$list,"end",i)
  tkselection.set(win1$var$list, 0)
  win1$rButton<- tk2button(win1$frame1, text = ">>",width=5)
  tkgrid(win1$rButton, padx = 5, pady = c(0,5),sticky="s",column=1,row=1)
  win1$lButton<- tk2button(win1$frame1, text = "<<",width=5)
  tkgrid(win1$lButton, padx = 5, pady = c(0,5),sticky="n",column=1,row=2)

  tkgrid(
    tk2label(
      win1$frame1, text = "Cluster Variables: \n(Min. 2 Variables)",
      justify = "left",
      font=fontCommands
    ),
    padx = 15, pady = c(5, 5),sticky="w",row=0,column=2
  )

  win1$var$cluster <-
    tk2listbox(
      win1$frame1,width = 25,height = 6,selectmode = "multiple"
    )
  tkgrid(win1$var$cluster, padx = 15, pady = c(0, 15),
         row=1,column=2,rowspan=2)

  onRight<-function(){
    tkdelete(win1$var$cluster,0,"end")
    var.choice<<- var[as.numeric(tkcurselection(win1$var$list))+1]
    for (i in var.choice)
      tkinsert(win1$var$cluster,"end",i)
    tkselection.set(win1$var$cluster, 0)
  }
  onLeft<-function(){
    var.remo.choice<<- var.choice[as.numeric(tkcurselection(win1$var$cluster))+1]
    tkdelete(win1$var$cluster,0,"end")
    var.choice<<-var.choice[-which(var.choice%in%var.remo.choice)]
    for (i in var.choice)
      tkinsert(win1$var$cluster,"end",i)
    tkselection.set(win1$var$cluster, 0)
  }
  tkconfigure(win1$rButton,command=onRight)
  tkconfigure(win1$lButton,command=onLeft)

  #--METHODS    OPTIONS------#
  win1$frame2<-tk2frame(win1,borderwidth=2,relief="flat")
  tkgrid(win1$frame2,padx=0,pady=c(0,0),row=2,column=0,sticky="w")
  fuzz.method <- c("Fuzzy C-Means",
                   "Gustafson Kessel")
  win1$method <-
    tk2combobox(win1$frame2,width = 30,values = fuzz.method)
  nCluster <- tk2spinbox(win1$frame2,width = 5,value = c(2:nrow(data)))
  mf<-seq(1,4,0.25)
  fuzzifier.param <-
    tk2spinbox(
      win1$frame2,width = 5,value = mf
    )
  gamma.param<- tk2spinbox(win1$frame2,width=5,value=seq(0,1,by=.1))
  SeedVal <- tclVar("0")
  SeedEntry <- tk2entry(win1$frame2, width = 5,
                        textvariable = SeedVal)
  ensemble.cek.value<-tclVar("0")
  ensemble.cek<-tk2checkbutton(win1$frame2,variable=ensemble.cek.value)
  n.ensemble<- tk2spinbox(win1$frame2,width=5,value=seq(10,100,by=10))
  tkgrid(
    tk2label(
      win1$frame2, text = "N Cluster:", justify = "left",
      font=fontCommands
    ),
    nCluster,
    tk2label(
      win1$frame2, text = "Fuzzifier:", justify = "left",
      font=fontCommands
    ),
    fuzzifier.param,
    tk2label(
      win1$frame2,text="Gamma:",justify="left",font=fontCommands
    ),
    gamma.param,
    padx = 10,pady = c(5,5),sticky="w"
  )
  tkgrid(
    tk2label(
      win1$frame2, text = "Method:", justify = "left",
      font=fontCommands
    ),
    win1$method,
    padx = 10,pady = c(5,5),sticky="w"
  )
  tkgrid(
    tk2label(
      win1$frame2, text = "Seed:", justify = "left",
      font=fontCommands
    ),
    padx = 10,pady = c(5,5),sticky="w",
    row=1,column=4
  )
  tkgrid(
    SeedEntry,row=1,column=5,padx = 10,pady = c(5,5),sticky="w"

  )
  tkgrid(tk2label(
    win1$frame2,text="Ensemble",justify="left", font=fontCommands
  ),ensemble.cek,
  tk2label(win1$frame2,text="N ensemble:",justify="left",font=fontCommands),
  n.ensemble,
  padx=10,pady=c(5,5),sticky="W"
  )
  method <- tclVar("Fuzzy C-Means")
  tkgrid.configure(win1$method,columnspan=3)
  tkconfigure(win1$method, textvariable = method)
  method.nCluster <- tclVar("2")
  tkconfigure(nCluster, textvariable = method.nCluster)
  method.fuzzifier <- tclVar("1")
  tkconfigure(fuzzifier.param, textvariable = method.fuzzifier)
  gamma.value<-tclVar("0")
  tkconfigure(gamma.param,textvariable=gamma.value)
  ensemble.seed<-tclVar("10")
  tkconfigure(n.ensemble,textvariable=ensemble.seed)

  tk2state.set(gamma.param,"disabled")
  OnImprove<-function(){
    if(tclvalue(method)=="Gustafson Kessel")
      tk2state.set(gamma.param,"normal")
    else
      tk2state.set(gamma.param,"disabled")
  }
  tkbind(win1$method,"<Button 1>",OnImprove)
  tk2state.set(n.ensemble,"disable")
  onCek<-function(){
    if(tclvalue(ensemble.cek.value)=="0")
    { tk2state.set(n.ensemble,"normal")
      }else{
      tk2state.set(n.ensemble,"disable")
    }
  }
  tkbind(ensemble.cek,"<Button 1>",onCek)

  win1$frame3<-tk2frame(win1,borderwidth=2,relief="flat")
  tkgrid(win1$frame3,padx=0,pady=c(0,0),row=3,column=0,sticky="w")
  nextButton<-tk2button(win1$frame3,text="Go>>",width="10")
  statuslabel<-tk2label(win1$frame3,text="STATUS: - ",
                        font=fontCommands,
                        justify="left")
  pb<-tk2progress(win1$frame3,orientation = "horizontal",
                  length=350,mode="determinate")
  tkgrid(
    nextButton,pb,
    padx = 5, pady = c(0,5),sticky="w")
  tkgrid(statuslabel,
         padx=5,
         column=1,sticky="w",
         pady=c(0,5))
  on.next<-function(){
    tcl("update")
    for(i in var.choice)
      data.cluster<<-cbind.data.frame(data.cluster,eval(parse(text=paste("data$",i,sep=""))))
    data.cluster<<-data.cluster[,-1]
    colnames(data.cluster)<<-var.choice
    rownames(data.cluster)<<-rownames(data)
    tkconfigure(pb,value=10)
    tkconfigure(statuslabel,text="STATUS: PROCESS CLUSTERING....")
    tcl("update")
    Sys.sleep(.5)
    if(tclvalue(ensemble.cek.value)=="0")
    {if(tclvalue(method)=="Fuzzy C-Means")
      cluster<<-fuzzy.CM(data.cluster,
                         K=as.numeric(tclvalue(method.nCluster)),
                         m=as.numeric(tclvalue(method.fuzzifier)),
                         RandomNumber=as.numeric(tclvalue(SeedVal)))
    else
      cluster<<-fuzzy.GK(data.cluster,
                         K=as.numeric(tclvalue(method.nCluster)),
                         m=as.numeric(tclvalue(method.fuzzifier)),
                         RandomNumber=as.numeric(tclvalue(SeedVal)),
                         gamma=as.numeric(tclvalue(gamma.value))

      )}else{
        if(tclvalue(method)=="Fuzzy C-Means"){
            cluster<<-soft.vote.ensemble(data = data.cluster,
                                         seed=as.numeric(tclvalue(ensemble.seed)),
                                         method = "FCM",
                                         K=as.numeric(tclvalue(method.nCluster)),
                                         m=as.numeric(tclvalue(method.fuzzifier)))
        }else {
            cluster<<-soft.vote.ensemble(data = data.cluster,
                                         seed=as.numeric(tclvalue(ensemble.seed)),
                                         method = "GK",
                                         K=as.numeric(tclvalue(method.nCluster)),
                                         m=as.numeric(tclvalue(method.fuzzifier)),
                                         gamma=as.numeric(tclvalue(gamma.value)))

        }
      }

    tkconfigure(pb,value=60)
    tkconfigure(statuslabel,text="STATUS: VALIDATING...")
    tcl("update")
    Sys.sleep(.5)
    valid<<-validation.index(cluster)
    tkconfigure(pb,value=80)
    tcl("update")
    Sys.sleep(.5)
    manov<<-try(checkManova(cluster),silent = T)
    tkconfigure(pb,value=90)
    tkconfigure(statuslabel,text="STATUS: PREPARING RESULT...")
    tcl("update")
    Sys.sleep(.5)
    tkconfigure(pb,value=100)
    tkconfigure(statuslabel,text="STATUS: FINISH :)")
    tcl("update")
    print("FINISH")
    if(tclvalue(ensemble.cek.value)!="0")
      methods=paste(tclvalue(method),"with Soft Voting Cluster Ensemble") else methods=tclvalue(method)
    result.GUI(win,cluster,valid,manov,methods)
  }
  tcl("ttk::style", "configure", "TFrame", background="white")
  tcl("ttk::style", "configure", "TLabel", background="white")
  tcl("ttk::style", "configure", "TButton",font=fontCommands)
  tcl("ttk::style", "configure", "TButton",background="white")
  tcl("ttk::style", "map", "TButton", background=c("active", "blue"))

  tcl("ttk::style", "configure","TPanedwindow", background="white")
  tcl("ttk::style", "configure","TProgressbar", troughcolor="blue")
  tkconfigure(nextButton,command=on.next)
  tkfocus(win1)
}
