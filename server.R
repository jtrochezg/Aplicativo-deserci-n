shinyServer(function(input,output,session){
  
  ##selecci?n de variables
  observe({
    # Direccion del archivo    
    inFile<-input$file
    
    if(is.null(inFile)) 
    return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1 = read.table(inFile$datapath, header=T)
    
    
    updateSelectInput(session, "edad", choices = names(data1))
    updateSelectInput(session, "facultad", choices = names(data1))
    updateSelectInput(session, "Programa", choices = names(data1))
    updateSelectInput(session, "tipo", choices = names(data1))
    updateSelectInput(session, "Encu", choices = names(data1))
    updateSelectInput(session, "Causa", choices = names(data1))
    updateSelectInput(session, "Razn", choices = names(data1))
   updateSelectInput(session, "Progra", choices = names(data1$Programa))
   updateSelectInput(session, "facu", choices = as.character(data1$facultad))
   updateSelectInput(session, "facu1", choices = as.character(data1$facultad))
   updateSelectInput(session, "nivel", choices = names(data1))
   updateSelectInput(session, "sede", choices = names(data1))
   updateSelectInput(session, "zona", choices = names(data1))
   updateSelectInput(session, "gen", choices = names(data1))
   updateSelectInput(session, "civil", choices = names(data1))
   updateSelectInput(session, "estrato", choices = names(data1))
  })
  
  observe({
    # Direccion del archivo    
  
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data2 = read.table("Cholesterol.txt", header=T)
    
    updateSelectInput(session, "Vrespuesta", choices = names(data2))
    updateSelectInput(session, "tiempo", choices = names(data2))
    updateSelectInput(session, "sujeto", choices = names(data2))
    updateSelectInput(session, "factor", choices = names(data2))
      })
  
  observe({
  
    data3 = read.table("Libro1.txt", header=T)
    updateSelectInput(session, "asig", choices = names(data3))
  })

  
  output$pred_plot20 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    uu3=datos %>%
      group_by(periodo)%>%
      summarise(sum(desertores))
    
    
    uu2=datos %>%
      group_by(periodo)%>%
      summarise(sum(mtriculados))
  
    ind=uu3$`sum(desertores)`/uu2$`sum(mtriculados)`
    datas=cbind(uu2,uu3$`sum(desertores)`,ind) 
    
    g <- ggplot(datas,aes(datas$periodo,datas$ind ))
    g + geom_col(fill=("skyblue"))+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.01),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.ticks = element_line(size =0.01),
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.text = element_text(size = 16),legend.position = "left",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      coord_cartesian(ylim = c(min(datas$ind), max(datas$ind)))+
      labs(y= "Índice de deserción",x= "Período")
    
  })
  
  output$pred_plot21 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
  
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
   
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
           xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  
  
  output$total_plot11 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(aes(y=..count../sum(..count..)),
                     stat = "count",position="dodge",fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  
  
  output$pred_plot22 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  
  
  output$total_plot12 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  
  
  
  output$pred_plot23 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })

  output$total_plot13 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",aes(y=..count../sum(..count..)),
                     position="dodge",fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  
  
  
  output$pred_plot24 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot14<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                     fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  
  output$pred_plot25 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[5]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot15<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[1]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[5]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                     fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  
  
  
  output$pred_plot26 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot16<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                     fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot27 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot17<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                     fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot28 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot18<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot29 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot19<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[2]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  
  
  
  output$pred_plot30<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot30<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot31<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot31<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot32<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot32<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot33<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot33<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  output$pred_plot34<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[5]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot34<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[5]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat ="count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot35<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[6]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot35<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[6]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                     fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  
  output$pred_plot36<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[7]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot36<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[7]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  output$pred_plot37<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[8]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot37<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[3]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[8]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })
  
  
  
  
  ####____________________________-
  output$pred_plot38<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot38<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[1]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  }) 
  
  
  output$pred_plot39<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot39<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[2]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })  
  
  
  output$pred_plot40<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot40<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[3]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })  
  
  output$pred_plot41<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot41<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[4]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  }) 
  
  
  output$pred_plot42<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[5]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot42<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[5]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })  
  
  
  output$pred_plot43<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[6]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot43<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[6]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })  
  
  
  output$pred_plot44<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[7]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia relativa")
    box()
    
    
  })
  
  output$total_plot44<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[7]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  }) 
  
  output$pred_plot45<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[8]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot45<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[8]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })  
  
  
  
  output$pred_plot46<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[9]]
    
    tcnart=na.omit(tcnart)
    
    rr2=data.frame(tcnart$pp,tcnart$caus,tcnart$raz)
    
    T1 <- table(tcnart$caus,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot((T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
    
    
  })
  
  output$total_plot46<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    yus=yu[[4]]
    
    attach(yu)
    
    pp=as.character(yus[,input$Programa])
    tip=as.character(yus[,input$tipo])
    caus=as.character(yus[,input$Causa])
    raz=as.character(yus[,input$Razn])
    
    rr=data.frame(tip,pp,caus,raz)
    
    artt=split(rr,pp)
    
    tcnart=artt[[9]]
    
    tcnart=na.omit(tcnart)
    
    colnames(tcnart) <- c("tipo3","programa3","Causa3","razon3")
    
    attach(tcnart)
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=tcnart,aes(x=reorder_size(razon3),fill=Causa3)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text.y = element_text(angle = 0, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causa3, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
    
  })  
  
  ####__________________________--
  
  
  
  
  
  
  
  output$tabless6 <- renderDataTable({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
   
     uu3=datos %>%
      group_by(periodo)%>%
      summarise(sum(desertores))
    
    
    uu2=datos %>%
      group_by(periodo)%>%
      summarise(sum(mtriculados))
    
    ind=uu3$`sum(desertores)`/uu2$`sum(mtriculados)`
    datas=data.frame(uu2,uu3$`sum(desertores)`,ind) 
    colnames(datas) <- c("Semestre","Matriculados","Desertores","Índice")
    datas
      })
  
  output$tabless7 <- renderDataTable({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T,dec=",")
    
    tmp <- hist(data1[, input$edad],breaks=(input$obs)-1,plot=FALSE)
    y=table.freq(tmp)
    y
    
    
    
  })
  
  
  output$pred_plot15 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    # Si s?lo queremos ver la parte inicial o final de la base usamos head o tail:
    head(datos) # para ver las primeros seis registros
    tail(datos) # para ver lo ?ltimos seis registros
    
    # Para disponer de las columnas de la base como variables como vectores hacemos
    attach(datos)
    uu=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    attach(datas)
    names(datas)
    
    ggplot(datas, aes(as.numeric(datas$periodo),datas$ind,colour=datas$facultad)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+  
      geom_line(lwd=1)+labs(x="Período" ,y= "Índice de deserción",color="Facultad")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
      })
  
  output$pred_plot47  <- renderPlot({

    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    # Si s?lo queremos ver la parte inicial o final de la base usamos head o tail:
    head(datos) # para ver las primeros seis registros
    tail(datos) # para ver lo ?ltimos seis registros
    
    # Para disponer de las columnas de la base como variables como vectores hacemos
    attach(datos)
    uu=datos %>%
      group_by(sede,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(sede,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    attach(datas)
    names(datas)
    
    ggplot(datas, aes(as.numeric(datas$periodo),datas$ind,colour=datas$sede)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.03),labels = scales::percent)+
      geom_line(lwd=1)+labs(x="Período" ,y= "Índice de deserción" ,color="Facultad")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = 14),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  output$pred_plot17<- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    names(datos)
    
    periodos=as.numeric(datos$periodo)
    indice=(desertores/mtriculados)
    datos=cbind(datos,indice,periodos)
    
    uu=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind) 
    
    attach(datas)
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[3]])
    
    
    plot(dataf2$periodo,dataf2$ind)
    t= data.frame(dataf2$periodo,dataf2$ind)
    
    g <- ggplot(t,aes(dataf2$periodo,dataf2$ind ))
    g + geom_col(fill=("skyblue"))+labs(y= "Índice de deserción",x= "Período")+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.01),labels = scales::percent)+
        theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = 16),legend.position = "bottom",
            legend.text = element_text(size = rel(1.5)),
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      coord_cartesian(ylim = c(min(dataf2$ind), max(dataf2$ind)))+
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  
  output$pred_plot18<- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    names(datos)
    
    periodos=as.numeric(datos$periodo)
    indice=(desertores/mtriculados)
    datos=cbind(datos,indice,periodos)
    
    uu=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind) 
    
    attach(datas)
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[2]])
    
    
    plot(dataf2$periodo,dataf2$ind)
    t= data.frame(dataf2$periodo,dataf2$ind)
    
    g <- ggplot(t,aes(dataf2$periodo,dataf2$ind ))
    g + geom_col(fill=("skyblue"))+labs(y= "Índice de deserción",x = "Período")+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.005),labels = scales::percent)+
      coord_cartesian(ylim = c(min(dataf2$ind), max(dataf2$ind)))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  
  output$pred_plot19<- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    names(datos)
    
    periodos=as.numeric(datos$periodo)
    indice=(desertores/mtriculados)
    datos=cbind(datos,indice,periodos)
    
    uu=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind) 
    
    attach(datas)
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[1]])
    
    
    plot(dataf2$periodo,dataf2$ind)
    t= data.frame(dataf2$periodo,dataf2$ind)
    
    g <- ggplot(t,aes(dataf2$periodo,dataf2$ind ))
    g + geom_col(fill=("skyblue"))+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.015),labels = scales::percent)+
      coord_cartesian(ylim = c(min(dataf2$ind), max(dataf2$ind)))+
      labs(y= "Índice de deserción",x= "Período")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  
  
  
  output$pred_plot48<- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    names(datos)
    
    periodos=as.numeric(datos$periodo)
    indice=(desertores/mtriculados)
    datos=cbind(datos,indice,periodos)
    
    uu=datos %>%
      group_by(sede,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(sede,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind) 
    
    attach(datas)
    
    dataf=split(datas,datas$sede)
    dataf2=(dataf[[2]])
  
    plot(dataf2$periodo,dataf2$ind)
    t= data.frame(dataf2$periodo,dataf2$ind)
    
    g <- ggplot(t,aes(dataf2$periodo,dataf2$ind ))
    g + geom_col(fill=("skyblue"))+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.03),labels = scales::percent)+
      coord_cartesian(ylim = c(min(dataf2$ind), max(dataf2$ind)))+
      labs(y= "Índice de deserción",x= "Período")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
  })
  
  
  
  
  
  
  
  output$pred_plot16<- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    names(datos)
    
    periodos=as.numeric(datos$periodo)
    indice=(desertores/mtriculados)
    datos=cbind(datos,indice,periodos)
    
    uu=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind) 
    
    attach(datas)
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[4]])
    
    
    plot(dataf2$periodo,dataf2$ind)
    t= data.frame(dataf2$periodo,dataf2$ind)
    
    g <- ggplot(t,aes(dataf2$periodo,dataf2$ind ))
    g + geom_col(fill=("skyblue"))+
      coord_cartesian(ylim = c(min(dataf2$ind), max(dataf2$ind)))+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.015),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      labs(y= "Índice de deserción",x = "Período")+
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  
  
  output$pred_plot1 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    

    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[1]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[1]]
    attach(ayhprof)
    
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
    
  })
  
  
  
  
  output$pred_plot49  <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    attach(datos)
    
    periodos=as.numeric(periodo)
    
    dataf=split(datos,sede)
    dataf2=(dataf[[2]])
    
    
    
    uu=dataf2 %>%
      group_by(dataf2$facultad,dataf2$periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    
    uu$sum.desertores.
    uu2=dataf2 %>%
      group_by(dataf2$facultad,dataf2$periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    matt=uu2$sum.mtriculados.
    datas=data.frame(uu,matt,ind) 
    datas$dataf2.periodo
    ggplot(datas, aes(as.numeric(datas$dataf2.periodo),datas$ind, colour = datas$dataf2.facultad)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.03),labels = scales::percent)+
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datas$dataf2.periodo))
    
    
      })
  
  output$pred_plot55  <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    head(datos)
    
    uu=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sumdesertores.
    uu2=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    
    datas=cbind(uu,mat,ind)  
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[1]])

    
    ggplot(dataf2, aes(as.numeric(dataf2$periodo),dataf2$ind, colour = dataf2$Beca)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(dataf2$periodo))
    
  
  })
  
  output$pred_plot56  <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    head(datos)
    
    uu=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sumdesertores.
    uu2=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    
    datas=cbind(uu,mat,ind)  
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[2]])
    
    
    ggplot(dataf2, aes(as.numeric(dataf2$periodo),dataf2$ind, colour = dataf2$Beca)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.01),labels = scales::percent)+
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(dataf2$periodo))
    
    
  })
  
  output$pred_plot57  <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    head(datos)
    
    uu=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sumdesertores.
    uu2=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    
    datas=cbind(uu,mat,ind)  
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[3]])
    
    
    ggplot(dataf2, aes(as.numeric(dataf2$periodo),dataf2$ind, colour = dataf2$Beca)) +
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(dataf2$periodo))
    
    
  })
  
  
  
  
  output$pred_plot58 <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    head(datos)
    
    uu=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sumdesertores.
    uu2=datos %>%
      group_by(facultad,periodo,Beca)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    
    datas=cbind(uu,mat,ind)  
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[4]])
    
    
    ggplot(dataf2, aes(as.numeric(dataf2$periodo),dataf2$ind, colour = dataf2$Beca)) +
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(dataf2$periodo))
    
    
  })
  
  
  
  
  output$pred_plot50  <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    
    dataf=split(datos,sede)
    dataf2=(dataf[[2]])
    
    attach(dataf2)
    
    
    uu=dataf2 %>%
      group_by(dataf2$facultad,dataf2$programa,dataf2$periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    
    uu$sum.desertores.
    uu2=dataf2 %>%
      group_by(dataf2$facultad,dataf2$programa,dataf2$periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    matt=uu2$sum.mtriculados.
    datas=data.frame(uu,matt,ind) 
    
    dataf3=split(datas,datas$dataf2.facultad)
    
    ayhprof=dataf3[[2]]
    attach(ayhprof)
    
    indice=ayhprof$desertores/ayhprof$mtriculados
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$dataf2.periodo),ayhprof$ind, colour = ayhprof$dataf2.programa)) +
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.05),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(ayhprof$dataf2.periodo)) 
   
  })
  
  
  output$pred_plot53 <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    names(datos)
    
    periodos=as.numeric(datos$periodo)
    indice=(desertores/mtriculados)
    datos=cbind(datos,indice,periodos)
    
    uu=datos %>%
      group_by(Beca,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(Beca,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind) 
    
    attach(datas)
    
    dataf=split(datas,datas$Beca)
    dataf2=(dataf[[1]])
    
    
    t= data.frame(dataf2$periodo,dataf2$ind)
    
    g <- ggplot(t,aes(dataf2$periodo,dataf2$ind ))
    g + geom_col(fill=("skyblue"))+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.01),labels = scales::percent)+
      coord_cartesian(ylim = c(min(dataf2$ind), max(dataf2$ind)))+
      labs(y= "Índice de deserción",x= "Período")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
  })
  
  
  
  output$pred_plot54 <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    uu=datos %>%
      group_by(Beca,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(Beca,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    attach(datas)
    names(datas)
    
    ggplot(datas, aes(as.numeric(datas$periodo),datas$ind,colour=datas$Beca)) +
      geom_line(lwd=1)+labs(x= "Período",y= "Índice de deserción",color="Facultad")+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.01),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = 14),legend.position = "bottom",
            strip.text.x = element_text(size=18),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  
    
  
  output$pred_plot51  <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    uu=datos %>%
      group_by(sede,facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu2=datos %>%
      group_by(sede,facultad,programa,Tipo,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    dataf=split(datas,datas$sede)
    dataf2=(dataf[[2]])
    
  
    dataf3=split(dataf2,dataf2$facultad)
    ayhprof=dataf3[[3]]
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.04),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
      })
  
  output$pred_plot52  <- renderPlot({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    
    attach(datos)
    
    
    dataf=split(datos,sede)
    dataf2=(dataf[[2]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$facultad)
    
    ayhprof=dataf3[[4]]
    attach(ayhprof)
    
    indice=ayhprof$desertores/ayhprof$mtriculados
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),indice, colour = ayhprof$programa)) +
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      scale_y_continuous(breaks=seq(min(indice),max(indice),0.04),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
  })
  
  output$pred_plot7 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[1]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[2]]
    attach(ayhprof)
    
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+
      geom_line(lwd=1)+labs(y= "Índice de deserción",x= "Período",color=" ")  +
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
    
    
  })
  
  output$pred_plot8 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    
    
    periodos=as.numeric(periodo)
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[2]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[1]]
    attach(ayhprof)
    
    ayhprof$ind
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.015),labels = scales::percent)+
      geom_line(lwd=1) +labs(y= "Índice de deserción",x= "Período",color = "Profesionales") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
            legend.title=element_text(size = rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = 14),legend.position = "bottom",
            legend.text = element_text(size = rel(1.5)),
            strip.text.x = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
  })
  
  output$pred_plot9 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    
    
    periodos=as.numeric(periodo)
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[2]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[2]]
    attach(ayhprof)
    
    ayhprof$ind
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      geom_line(lwd=1) +labs(y= "Índice de deserción",x= "Período",color = "Tecnologías") +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.015),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
            legend.title=element_text(size = rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = 14),legend.position = "bottom",
            legend.text = element_text(size = rel(1.5)),
            strip.text.x = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
    
    
  })
  
  output$pred_plot10 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu2=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[3]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[1]]
    attach(ayhprof)
    
    ayhprof$ind
    
  
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      geom_line(lwd=1)  +labs(y= "Índice de deserción",x= "Período",color=" ")+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.025),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            legend.title=element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
    
    
  })
  
  output$pred_plot11 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[3]])
    
    attach(dataf2)
    
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[2]]
    attach(ayhprof)
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.015),labels = scales::percent)+
      geom_line(lwd=1)  +labs(y= "Índice de deserción",x = "Período",color = "Tecnologías")+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
    
    
  })
  
  
  output$pred_plot12 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    
    attach(datos)
    
    
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu2=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[4]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[1]]
    attach(ayhprof)
    
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      geom_line(lwd=1)  +labs(y="Índice de deserción",x= "Período",color=" ")+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.03),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            legend.text = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
    
    
    
    
  })
  
  output$pred_plot13 <- renderPlot({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
    uu=datos %>%
      group_by(facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    periodos=as.numeric(periodo)
    
    
    dataf=split(datas,datas$facultad)
    dataf2=(dataf[[4]])
    
    attach(dataf2)
    dataf3=split(dataf2,dataf2$Tipo)
    ayhprof=dataf3[[2]]
    attach(ayhprof)
    
    indice=ayhprof$desertores/ayhprof$mtriculados
    
    ggplot(ayhprof, aes(as.numeric(ayhprof$periodo),ayhprof$ind, colour = ayhprof$programa)) +
      geom_line(lwd=1)  +labs(y= "Índice de deserción",x= "Período",color="Tecnologías")+
      scale_y_continuous(breaks=seq(min(datas$ind),max(datas$ind),0.02),labels = scales::percent)+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),legend.position = "bottom",
            strip.text.x = element_text(size=16),
            legend.text = element_text(size = rel(1.5)),
            legend.title=element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      scale_x_discrete(limits=levels(datos$periodo))
          })
  
  
  ########indicesssssssss_________________________________________
  output$contents99 <- renderDataTable({
    
    datos=read.table("Cholesterol.txt", header=T)
   
    
    attach(datos)
    uu=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,programa,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    colnames(datas) <- c("Facultad", "Programa","Semestre","Desertores","Matriculados","Índice")
    datas
    
  })
  
  
  
  
  
  
  output$contents100<- renderDataTable({
    
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    attach(datos)
   periodos=as.numeric(periodo)
    
    dataf=split(datos,sede)
    dataf2=(dataf[[2]])
     uu=dataf2 %>%
      group_by(dataf2$facultad,dataf2$periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    
    uu$sum.desertores.
    uu2=dataf2 %>%
      group_by(dataf2$facultad,dataf2$periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    uu2$sum.mtriculados.
    
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    matt=uu2$sum.mtriculados.
    datas=data.frame(uu,matt,ind) 
  
    colnames(datas) <- c("Facultad","Semestre","Desertores","Matriculados","Índice")
       datas
  })
  
  
  
  
  
  
  
  output$tabless1 <- renderDataTable({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
  
    # Para disponer de las columnas de la base como variables como vectores hacemos
    attach(datos)
    uu=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(facultad,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    colnames(datas) <- c("Facultad","Semestre","Desertores","Matriculados","Índice")
    datas
    
  })
  
  
  
  
  
  output$tabless13  <- renderDataTable({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    
    # Si s?lo queremos ver la parte inicial o final de la base usamos head o tail:
    head(datos) # para ver las primeros seis registros
    tail(datos) # para ver lo ?ltimos seis registros
    
    # Para disponer de las columnas de la base como variables como vectores hacemos
    attach(datos)
    uu=datos %>%
      group_by(sede,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(sede,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    colnames(datas) <- c("Sede","Semestre","Desertores","Matriculados","Índice")
    datas
    
  })
  
  
  
  output$tabless14  <- renderDataTable({
    
    datos <- read.delim("Cholesterol.txt",header=T,dec=".",sep = "\t")
    uu=datos %>%
      group_by(sede,facultad,programa,Tipo,periodo)%>%
      summarise(sum(desertores))
    
    uu=data.frame(uu)
    
    uu$sum.desertores.
    uu2=datos %>%
      group_by(sede,facultad,programa,Tipo,periodo)%>%
      summarise(sum(mtriculados))
    uu2=data.frame(uu2)
    
    mat=uu2$`sum(mtriculados)`
    ind=uu$sum.desertores./uu2$sum.mtriculados.
    datas=cbind(uu,uu2$sum.mtriculados.,ind)  
    
    
    dataf=split(datas,datas$sede)
    dataf2=(dataf[[2]])
    
    colnames(dataf2) <- c("Sede","Facultad", "Programa","Tipo","Periodo","Desertores","Matriculados","Índice")
    dataf2[-1]
    dataf2
  })
  
  
  
  ##tabla incial
  output$table <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
  })
  
  #tabla de causas de desercipn a nivel global

  
  
  output$tabless9 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    data1  
    t2=ftable(data1[, input$gen],row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Genero","Conteo","Proporcion")
    tt22
    
  })
  
  
  
  output$tabless8 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    data1  
    t2=ftable(data1[, input$civil],row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("estado civil","Conteo","Proporcion")
    tt22
    
  })
  
  output$tabless11 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    data1  
    t2=ftable(data1[, input$tipo],row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Tipo","Conteo","Proporcion")
    tt22
    
  })
  
  output$tabless12 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    data1  
    t2=ftable(data1[, input$estrato],row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Estrato","Conteo","Proporcion")
    tt22
    
  })
  
  

  ###______________________________________________________________
  ###_____________________________
  ### GRAFICO DE CAUSAS DE deserción A NIVEL GLOBAL
  output$pred_plot <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
    
    T1 <- table(data1[, input$Razn],data1[, input$Causa],useNA = "no")
    T1 <- prop.table(T1)
    
    barplot(colSums(T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia relativa" )
    box()
  })
  
  
  
  ### GRAFICO DE CAUSAS DE deserción A NIVEL GLOBAL
  output$pred_plot6 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
  
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    zonas= data1[, input$zona]
    data=data.frame(zonas)
    
    ttt=na.omit(data)
    
    ggplot(ttt,aes(reorder_size(ttt$zonas)))+
      geom_bar(stat = "count",position="dodge",fill=("skyblue"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=rel(1.2)), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99")) +
      labs(x = "Zona de vivienda",y = "Frecuencia absoluta")
     
  })
  
  
  output$pred_plot14<- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
    
    edads=as.numeric(data1[, input$edad])
    
   
    tmp <- hist(edads, breaks=(input$obs)-1,col="skyblue",border="gray",main="",
                xlab="Edad",cex.lab=1.5,ylab="Frecuencia absoluta",cex.axis =2)
    box()
  })
  

  output$tabless10<- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    data1  
    t2=ftable(data1[, input$zona],row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("zona","Conteo","Proporcion")
    tt22
    
  })
  
  
    
  
    ### tabla de CAUSAS Y RAZONES DE deserción A NIVEL GENERAL
    output$contents_dv <- renderDataTable({
     
      #lectura de datos
      inFile <- input$file
      if (is.null(inFile))
      return(NULL)
      data1=read.table(inFile$datapath, header=T)
     
     #tabla de datos
      t2=ftable(data1[, input$Causa],data1[, input$Razn],exclude=0,row.vars = 1:2)
      t2=data.frame(t2)
     #t2
     ##funcion para convertir 0 en NA
      haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
     
     #pasos para quitar 0 de los datos
     dataf.2=data.frame(sapply(t2[3],haz.cero.na))
     tt=data.frame(t2,dataf.2)
     #tt
     ttt=na.omit(tt)
     #ttt
     tt1=ttt[-4]
     tt11=data.frame(cbind(tt1,round(tt1[3]/sum(tt1[3])*100,2)))
       colnames(tt11) <- c("Causa","Razon","Conteo","Proporcion")
       tt11
   
     
       })
   
    ##### TAMAÑOS DE MUESTRA
    output$contents22 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    
    # Lectura del archivo con direccion 
    #inFile$datapath y almacenamiento en data1
    
    data1=read.table(inFile$datapath, header=T)
    
    #tabla de datos
    t2=ftable(data1[, input$facultad],data1[, input$Programa],exclude=0,row.vars = 1:2)
    t2=data.frame(t2)
    #t2
    ##funcion para convertir 0 en NA
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    #pasos para quitar 0 de los datos
    dataf.2=data.frame(sapply(t2[3],haz.cero.na))
    tt=data.frame(t2,dataf.2)
    #tt
    ttt=na.omit(tt)
    #ttt
    tt1=ttt[-4]
    
    prop=round(ttt[3]/sum(ttt[3])*100,2)
    prop2=((tt1[3]/sum(tt1[3])))
    total=sum(tt1[3])
    
    
    hh=as.numeric(((1-input$bins2)/2))
    z=qnorm(hh, mean = 0, sd = 1)
    
    total=sum(ttt[3])
    prop2
    error=input$bins1
    q=(1-prop2)
    num=(total*prop2*q*(z^2))
    den=(error^2*(total-1))+(prop2*(1-prop2)*(z^2))
    tamao=round(num/den)
    
    #tt11=cbind(tt1,prop,tamao)
  #tt11

  })
    
    
    

    ###aNALISIS POR FACULTAD
    ##niveles de deserción
    ##FACULTAD DE ARTES
    output$tabless22 <- renderPrint({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[1]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      t2=table(Nivel,tipo)
      t3=prop.table(round(t2),2)
      t4=addmargins(t3)
      
      kable(round(t4,4)*100)
      
                })
    output$total_plot66 <- renderPlot({
      
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[1]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      #-------
      datas=data.frame(Nivel,tipo)
      
      ttt=na.omit(datas)
      ttt
      
      ggplot(data=ttt,aes(x=Nivel),fill=tipo) +
       geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                       fill=("skyblue"))+ 
        scale_y_continuous(labels = scales::percent)+  
        theme(axis.text.x = element_text(angle = 0, hjust = 1,size=16),
              axis.text = element_text(size = 12),legend.position = "left",
              strip.text.x = element_text(size=16),
              axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
              axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
              legend.text = element_text(size = rel(1.5)),
              panel.background = element_rect(fill = "grey95", colour = "grey50"),
              strip.background = element_rect(colour = "black", fill = "grey99"))+
        facet_grid(~tipo, scales="free", space="free")+
        scale_y_continuous(labels = scales::percent)+  
        labs(x = "Semestre de deserción",y = "Frecuencia relativa")
        
    })
    
   
    ##FACULTAD DE ARTES
    output$tabless23 <- renderPrint({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[3]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      t2=table(Nivel,tipo)
      t3=prop.table(round(t2),2)
      t4=addmargins(t3)
      
      kable(round(t4,4)*100)
      
    })
    output$total_plot67 <- renderPlot({
      
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[3]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      #-------
      datas=data.frame(Nivel,tipo)
      
      ttt=na.omit(datas)
      ttt
      
      ggplot(data=ttt,aes(x=Nivel),fill=tipo) +
        geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                       fill=("skyblue"))+
        scale_y_continuous(labels = scales::percent)+  
        theme(axis.text.x = element_text(angle = 0, hjust = 1,size=16),
              axis.text = element_text(size = 12),legend.position = "left",
              strip.text.x = element_text(size=16),
              axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
              axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
              legend.text = element_text(size = rel(1.5)),
              panel.background = element_rect(fill = "grey95", colour = "grey50"),
              strip.background = element_rect(colour = "black", fill = "grey99"))+
        facet_grid(~tipo, scales="free", space="free")+
        labs(x = "Semestre de deserción según el tipo",y = "Frecuencia relativa")
    })
    
    output$tabless24 <- renderPrint({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[2]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      t2=table(Nivel,tipo)
      t3=prop.table(round(t2),2)
      t4=addmargins(t3)
      
      kable(round(t4,4)*100)
      
    })
    output$total_plot68 <- renderPlot({
      
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[2]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      #-------
      datas=data.frame(Nivel,tipo)
      
      ttt=na.omit(datas)
      ttt
      
      ggplot(data=ttt,aes(x=Nivel),fill=tipo) +
        geom_histogram(stat = "count",aes(y=..count../sum(..count..)),
                       position="dodge",fill=("skyblue"))+
        scale_y_continuous(labels = scales::percent)+  
        theme(axis.text.x = element_text(angle = 0, hjust = 1,size=16),
              axis.text = element_text(size = 12),legend.position = "left",
              strip.text.x = element_text(size=16),
              axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
              axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
              legend.text = element_text(size = rel(1.5)),
              panel.background = element_rect(fill = "grey95", colour = "grey50"),
              strip.background = element_rect(colour = "black", fill = "grey99"))+
        facet_grid(~tipo, scales="free", space="free")+
          labs(x = "Semestre de deserción según el tipo",y = "Frecuencia relativa")
    })
    
    
    output$tabless25 <- renderPrint({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[4]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      t2=table(Nivel,tipo)
      t3=prop.table(round(t2),2)
      t4=addmargins(t3)
      
      kable(round(t4,4)*100)
      
    })
    output$total_plot69 <- renderPlot({
      
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      
      data1=read.table(inFile$datapath, header=T)
      yu=split(data1,data1[, input$facultad])
      
      
      AYH= yu[[4]]
      #AYH=yu$Artes_y_humanidades
      
      attach(AYH)
      
      Nivel=as.factor(AYH[, input$nivel])
      tipo=as.factor(AYH[,input$tipo])
      
      #-------
      datas=data.frame(Nivel,tipo)
      
      ttt=na.omit(datas)
      ttt
      
      ggplot(data=ttt,aes(x=Nivel),fill=tipo) +
        geom_histogram(stat = "count",aes(y=..count../sum(..count..)),
                       position="dodge",fill=("skyblue"))+
        scale_y_continuous(labels = scales::percent)+  
        theme(axis.text.x = element_text(angle = 0, hjust = 1,size=16),
              axis.text = element_text(size = 12),legend.position = "left",
              strip.text.x = element_text(size=16),
              axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
              axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
              legend.text = element_text(size = rel(1.5)),
              panel.background = element_rect(fill = "grey95", colour = "grey50"),
              strip.background = element_rect(colour = "black", fill = "grey99"))+
        facet_grid(~tipo, scales="free", space="free")+
        labs(x = "Semestre de deserción según el tipo de programa",y = "Frecuencia relativa")
    })
    
    
    ##causas por facultad
  output$tabless2 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[1]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Causa","Conteo","Proporcion")
    tt22
    
  })
  
  
  
  output$tabless <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    Causas = as.factor(data1[, input$Causa])
    
    
    t2=ftable(Causas,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Causa","Conteo","Proporcion")
    tt22
    
  })
  
    ##TABLA DE CAUSA Y RZNS EN LA FACULTAD DE ARTES
  
  output$tablel <- renderDataTable ({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion 
    data1=read.table(inFile$datapath, header=T)
    
    # facus <- data1[data1$facultad == input$facu,]
     
    yu=split(data1,data1[, input$facultad])
   
    AYH= yu[[1]]
    #AYH=yu$Artes_y_humanidades
  
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
  
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    t2
    haz.cero.na=function(x){
    ifelse(x==0,NA,x)}
    
    dataf.2=data.frame(sapply(t2[3],haz.cero.na))
    tt=data.frame(t2,dataf.2)
    ttt=na.omit(tt)
    ttt=ttt[-4]
    
    prop=round(ttt[3]/sum(ttt[3])*100,2)
    tt11=cbind(ttt,prop)
    tt22=data.frame(tt11[order(tt11$Causas),])
    colnames(tt22) <- c("Causa","Raz?n","Conteo","Proporci?n")
    sum(tt22$Conteo)
    tt22
    
      })
  
  ##GRAFICO DE CAUSAS POR LA FACULTAD DE ARTES
  
  output$pred_plot2 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[1]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    
    T1 <- table(Razns,Causas,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot(colSums(T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
  })
  
  ### grafico 2__________________GRAFICO DE-razones  ARTESSSSSSSSS
  output$total_plot2 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[1]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    #-------
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    
    
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    
    datas=data.frame(Causas, Razns)
    
    ttt=na.omit(datas)
    ttt
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=ttt,aes(x=reorder_size(Razns),fill=Causas)) +
      geom_histogram(stat = "count",position="dodge",fill=("skyblue"), 
                     aes(y=..count../sum(..count..)))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
            axis.text = element_text(size = 10),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causas, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  ### grafico 2__________________GRAFICO DE-razones  ARTESSSSSSSSS
  output$total_plot6 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    Nivel= as.factor(data1[, input$nivel])
    tipo = as.factor(data1[, input$tipo])
    
    #-------
    datas=data.frame(Nivel,tipo)
    
    ttt=na.omit(datas)
    ttt
    
    ggplot(data=ttt,aes(x=Nivel),fill=tipo) +
      geom_histogram(stat = "count", aes(y=..count../sum(..count..)),
                     position="dodge",fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.2)),
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~tipo, scales="free", space="free")+
      labs(x = "Semestre de deserción según el tipo de programa",y = "Frecuencia relativa")
  })
  
 
  output$total_plot7 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    Genero= as.factor(data1[, input$gen])
    

    
    
    #-------
    datas=data.frame(Genero)
    
    ttt=na.omit(datas)
    
    
    ggplot(data=ttt,aes(x=Genero)) +
      geom_bar(stat = "count",position="dodge",fill=("skyblue"))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      labs(x = "Género",y = "Frecuencia absoluta")
  })
  
  output$total_plot8 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    civil= as.factor(data1[, input$civil])
    
    
    
    
    #-------
    datas=data.frame(civil)
    
    ttt=na.omit(datas)
    
    
    ggplot(data=ttt,aes(x=civil)) +
      geom_bar(stat = "count",position="dodge",fill=("skyblue"))+
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      labs(x = "Estado civil",y = "Frecuencia absoluta")
    
  })
  
  
  output$total_plot9 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    tipo= as.factor(data1[, input$tipo])

    datas=data.frame(tipo)
    
    ttt=na.omit(datas)
    
    
    ggplot(data=ttt,aes(x=tipo)) +
      geom_bar(stat = "count",position="dodge",fill=("skyblue"))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
    labs(x = "Deserción según el tipo de programa",y = "Frecuencia absoluta")
  })
  
  
  output$total_plot10<- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    estrato= as.factor(data1[, input$estrato])
    
    datas=data.frame(estrato)
    
    ttt=na.omit(datas)
    
    
    ggplot(data=ttt,aes(x=estrato)) +
      geom_bar(stat = "count",position="dodge",fill=("skyblue"))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      labs(x = "Estrato",y = "Frecuencia absoluta")
    
  })
  
  
  output$tablel3 <- renderDataTable ({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion 
    data1=read.table(inFile$datapath, header=T)
    
    # facus <- data1[data1$facultad == input$facu,]
    
    yu=split(data1,data1[, input$facultad])
    
    AYH= yu[[3]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    t2
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    dataf.2=data.frame(sapply(t2[3],haz.cero.na))
    tt=data.frame(t2,dataf.2)
    ttt=na.omit(tt)
    ttt=ttt[-4]
    
    prop=round(ttt[3]/sum(ttt[3])*100,2)
    tt11=cbind(ttt,prop)
    tt22=data.frame(tt11[order(tt11$Causas),])
    colnames(tt22) <- c("Causa","Raz?n","Conteo","Proporci?n")
    sum(tt22$Conteo)
    tt22
    
  })
  
  ############FINAL FACULTAD DE ARTES
  
  #############INICIO ECONOMICAS
  
  output$pred_plot3 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[3]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    
    T1 <- table(Razns,Causas,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot(colSums(T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
  })
  
  
  ##causas por facultad
  output$tabless3 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[3]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Causa","Conteo","Proporcion")
    tt22
    
  })
  
  ### grafico 2__________________GRAFICO DE-razones  ARTESSSSSSSSS
  output$total_plot3 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[3]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    #-------
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    
    
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    
    datas=data.frame(Causas, Razns)
    
    ttt=na.omit(datas)
    ttt
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=ttt,aes(x=reorder_size(Razns),fill=Causas)) +
      geom_histogram(stat = "count",position="dodge",aes(y=..count../sum(..count..)),
                     fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causas, scales="free", space="free")+
      labs(   x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  output$tablel4 <- renderDataTable ({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion 
    data1=read.table(inFile$datapath, header=T)
    
    # facus <- data1[data1$facultad == input$facu,]
    
    yu=split(data1,data1[, input$facultad])
    
    AYH= yu[[3]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    t2
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    dataf.2=data.frame(sapply(t2[3],haz.cero.na))
    tt=data.frame(t2,dataf.2)
    ttt=na.omit(tt)
    ttt=ttt[-4]
    
    prop=round(ttt[3]/sum(ttt[3])*100,2)
    tt11=cbind(ttt,prop)
    tt22=data.frame(tt11[order(tt11$Causas),])
    colnames(tt22) <- c("Causa","Raz?n","Conteo","Proporci?n")
    sum(tt22$Conteo)
    tt22
    
  })
  
  ###########FINAL ECONOMICAS
  
  
  
  #############INICIO exactas
  
  output$pred_plot4 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[2]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    
    T1 <- table(Razns,Causas,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot(colSums(T1),col = 'skyblue', border = 'white',cex.axis =2,cex.names=2,
            xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
  })
  
  
  ##causas por facultad
  output$tabless4 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[2]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Causa","Conteo","Proporcion")
    tt22
    
  })
  
  ### grafico 2__________________GRAFICO DE-razones  ARTESSSSSSSSS
  output$total_plot4 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[2]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    #-------
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    
    
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    
    datas=data.frame(Causas, Razns)
    
    ttt=na.omit(datas)
    ttt
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=ttt,aes(x=reorder_size(Razns),fill=Causas)) +
      geom_histogram(stat = "count",aes(y=..count../sum(..count..)),
                     position="dodge",fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causas, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  
  
  output$tablel4 <- renderDataTable ({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion 
    data1=read.table(inFile$datapath, header=T)
    
    # facus <- data1[data1$facultad == input$facu,]
    
    yu=split(data1,data1[, input$facultad])
    
    AYH= yu[[2]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    t2
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    dataf.2=data.frame(sapply(t2[3],haz.cero.na))
    tt=data.frame(t2,dataf.2)
    ttt=na.omit(tt)
    ttt=ttt[-4]
    
    prop=round(ttt[3]/sum(ttt[3])*100,2)
    tt11=cbind(ttt,prop)
    tt22=data.frame(tt11[order(tt11$Causas),])
    colnames(tt22) <- c("Causa","Raz?n","Conteo","Proporci?n")
    sum(tt22$Conteo)
    tt22
    
  })
  ###########FINAL exactas
  
  
  #############INICIO ingenierias
  output$tablel5 <- renderDataTable ({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion 
    data1=read.table(inFile$datapath, header=T)
    
    # facus <- data1[data1$facultad == input$facu,]
    
    yu=split(data1,data1[, input$facultad])
    
    AYH= yu[[4]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    t2
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    dataf.2=data.frame(sapply(t2[3],haz.cero.na))
    tt=data.frame(t2,dataf.2)
    ttt=na.omit(tt)
    ttt=ttt[-4]
    
    prop=round(ttt[3]/sum(ttt[3])*100,2)
    tt11=cbind(ttt,prop)
    tt22=data.frame(tt11[order(tt11$Causas),])
    colnames(tt22) <- c("Causa","Raz?n","Conteo","Proporci?n")
    sum(tt22$Conteo)
    tt22
    
  })
  
  output$pred_plot5 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[4]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    
    T1 <- table(Razns,Causas,useNA = "no")
    T1 <- prop.table(T1)
    
    barplot(colSums(T1),col = 'skyblue', border = 'white',cex.axis =2,
            cex.names=2,xlab="Causas de deserción",cex.lab=1.5,ylab="Frecuencia")
    box()
  })
  
  
  ##causas por facultad
  output$tabless5 <- renderDataTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[4]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    t2=ftable(Causas,row.vars = 1)
    t2=data.frame(t2)
    prop=round(t2[2]/sum(t2[2])*100,2)
    tt11=cbind(t2,prop)
    tt22=data.frame(tt11[ordered(tt11$Freq),])
    colnames(tt22) <- c("Causa","Conteo","Proporcion")
    tt22
    
  })
  
  ### grafico 2__________________GRAFICO DE-razones  ARTESSSSSSSSS
  output$total_plot5 <- renderPlot({
    
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    data1=read.table(inFile$datapath, header=T)
    
    yu=split(data1,data1[, input$facultad])
    
    
    AYH= yu[[4]]
    #AYH=yu$Artes_y_humanidades
    
    attach(AYH)
    
    Causas = as.factor(AYH[, input$Causa])
    Razns = as.factor(AYH[, input$Razn])
    
    #-------
    
    t2=ftable(Causas,Razns,row.vars = 1:2)
    t2=data.frame(t2)
    
    
    haz.cero.na=function(x){
      ifelse(x==0,NA,x)}
    
    
    datas=data.frame(Causas, Razns)
    
    ttt=na.omit(datas)
    ttt
    
    reorder_size <- function(x) {
      factor(x, levels = names(sort(table(x),decreasing = TRUE)))
    }
    ###%in%levels(ttt$razon)[table(ttt$razon)>1]
    ggplot(data=ttt,aes(x=reorder_size(Razns),fill=Causas)) +
      geom_histogram(stat = "count", aes(y=..count../sum(..count..)),
                     position="dodge",fill=("skyblue"))+
      scale_y_continuous(labels = scales::percent)+  
      theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
            axis.text.y = element_text(hjust = 1,size=rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
            axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
            legend.text = element_text(size = rel(1.5)),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      facet_grid(~Causas, scales="free", space="free")+
      labs(x = "Razones de deserción",y = "Frecuencia relativa")
  })
  
  ###########FINAL ingenierias
  
  ### grafico 2__________________-razones  ITM______________
     output$total_plot <- renderPlot({
        
        inFile <- input$file
        if (is.null(inFile))
        return(NULL)
        data1=read.table(inFile$datapath, header=T)
        
        Causas = as.factor(data1[, input$Causa])
        Razns = as.factor(data1[, input$Razn])
        
        
        #-------
        
        t2=ftable(Causas,Razns,row.vars = 1:2)
        t2=data.frame(t2)
        
        
          haz.cero.na=function(x){
          ifelse(x==0,NA,x)}
        
        
        datas=data.frame(Causas, Razns)
        
        ttt=na.omit(datas)
        ttt
        
        reorder_size <- function(x) {
          factor(x, levels = names(sort(table(x),decreasing = TRUE)))
        }
        ###%in%levels(ttt$razon)[table(ttt$razon)>1]
        ggplot(data=ttt,aes(x=reorder_size(Razns),fill=Causas)) +
          geom_histogram(stat = "count",position="dodge",fill=("skyblue"),
                         aes(y=..count../sum(..count..)))+
          scale_y_continuous(labels = scales::percent)+  
          theme(axis.text.x = element_text(angle = 60, hjust = 1,size=rel(1.5)),
                axis.text.y = element_text(hjust = 1,size=rel(1.5)),
                axis.text = element_text(size = 12),legend.position = "left",
                strip.text.x = element_text(size=16),
                axis.title.x =element_text(vjust=-0.5,size=rel(2.5)),
                axis.title.y = element_text(vjust=1.5,size=rel(2.5)),
                legend.text = element_text(size = rel(1.5)),
                panel.background = element_rect(fill = "grey95", colour = "grey50"),
                strip.background = element_rect(colour = "black", fill = "grey99"))+
          facet_grid(~Causas, scales="free", space="free")+
          labs(x = "Razones de deserción",y = "Frecuencia relativa")
               })
     
     output$summary3 <- renderPrint({
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       
       data1=read.table(inFile$datapath, header=T)
       
       Causas = as.factor(data1[, input$Causa])
       Razns = as.factor(data1[, input$Razn])
       facultad=as.factor(data1[, input$facultad])
      
       
       yy=table(Causas,facultad)
       u=prop.table(yy,2)
       u=addmargins(u,1)
       
       kable(round(u,4)*100)
            })
     
       # 
     output$summar <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
             data1=read.table(inFile$datapath, header=T)
           
             Causas = as.factor(data1[, input$Causa])
             Razns = as.factor(data1[, input$Razn])
             facu=as.factor(data1[, input$facultad])
             
             yy=table(Razns,facu)
             u=prop.table(yy,2)
             u=addmargins(u,1)
             
             kable(round(u,4)*100)
             
             
     })
     
     ########tabla de causas comparativas tecnologias
     
     output$summar5 <- renderPrint({
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[1]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
      raz=as.character(yus[,input$Razn])
       
       
       
      
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       artt
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       tcnart
       pro=as.character(tcnart$pp)
     
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
        u=addmargins(t2,1)
       
       kable(round(u,4)*100)
       
       
         })
     ##
     ######tabla de causas comparativas tecnologias
     
     output$summar9 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[3]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       
       
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       artt
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       tcnart
       pro=as.character(tcnart$pp)
       
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       u=addmargins(t2,1)
       
       kable(round(u,4)*100)
       
       
     })
     output$summar13 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[2]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       artt
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       tcnart
       pro=as.character(tcnart$pp)
       
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       u=addmargins(t2,1)
       
       kable(round(u,4)*100)
       
       
     })
     output$summar17 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[4]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       artt
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       tcnart
       pro=as.character(tcnart$pp)
       
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       u=addmargins(t2,1)
       
       kable(round(u,4)*100)
       
       
     })
     ########tabla de razones comparativas tecnologias
     
     output$summar7 <- renderPrint({
       
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[1]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
      
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
           })
     
     
     output$summar10 <- renderPrint({
       
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[3]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
     })
     output$summar14 <- renderPrint({
       
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[2]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
     })
     output$summar18 <- renderPrint({
       
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       yu=split(data1,data1[, input$facultad])
       
       yus=yu[[4]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[2]]
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
     })
     #######tabla de causas comparativas profesionales
     
     output$summar6 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       yus=yu[[1]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
      
     })
     output$summar11 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       yus=yu[[3]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
     })
     output$summar15 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       yus=yu[[2]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
     })
     output$summar19 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       yus=yu[[4]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$caus,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
     })
     output$summar8 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       
       yus=yu[[1]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
       
       
     })
     output$summar12 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       yus=yu[[3]]
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       u=addmargins(t2,1)
       kable(round(u,3)*100)
       
       })
     
     
     
     
     output$summar16 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       
       yus=yu[[2]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
       
       
     })
     output$summar20 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       facu=as.factor(data1[, input$facultad])
       
       yu=split(data1,facu)
       
       yus=yu[[4]]
       
       attach(yu)
       
       pp=as.character(yus[,input$Programa])
       tip=as.character(yus[,input$tipo])
       caus=as.character(yus[,input$Causa])
       raz=as.character(yus[,input$Razn])
       
       rr=data.frame(tip,pp,caus,raz)
       
       artt=split(rr,tip)
       tcnart=artt[[1]]
       
       
       tcnart=na.omit(tcnart)
       pro=as.character(tcnart$pp)
       
       t2=table(tcnart$raz,pro)
       t2=prop.table(t2,2)
       
       u=addmargins(t2,1)
       
       kable(round(u,3)*100)
       
       
       
     })
     
     output$summar21 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       Nivel=as.factor(data1[, input$nivel])
       tipo=as.factor(data1[,input$tipo])
       
       t2=table(Nivel,tipo)
       t3=prop.table(round(t2),2)
       t4=addmargins(t3)
       kable(round(t4,4)*100)
       
       
     })
   
     
     ###ojoooooo
     output$summar50 <- renderPrint({
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       genero=data1[,input$gen]
     Sede=(data1[, input$sede])
      
       
       
       t2=table(genero,Sede)
       t3=prop.table(round(t2),2)
       t4=addmargins(t3)
       kable(round(t4,4)*100)
       
     })
     
     #######REACTIVEEEEE__________________________________
     sliderValues <- reactive({
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
          #tabla de datos
       t2=ftable(data1[, input$facultad],row.vars = 1)
       t2=data.frame(t2)
       
       prop=round(t2[2]/sum(t2[2])*100,2)
       prop2=((t2[2]/sum(t2[2])))
       total=sum(t2[2])
       
       hh=as.numeric(((1-input$bins2)/2))
       z=qnorm(hh, mean = 0, sd = 1)
       
       #z=1.96
       total=sum(t2[2])
       prop2
       error=input$bins1
       q=(1-prop2)
       
       num=(sum(t2[2]*sqrt(prop2*q)))^2
       den1=sum((total*error/z)^2)
       den2=sum(t2[2]*prop2*q)
       
       
       tamao=round(num/(den1+den2),0)
       
       ###Encuestados
       t2t=ftable(data1[, input$Programa],data1[, input$Encu],exclude=0,row.vars = 1:2)
       t2t=data.frame(t2t)
       
       vec=t2t$Freq
       ini=(length(vec)/2)+1
       
       vec2=vec[ini:length(vec)]
       rrr=sum(vec2)
       
       
       
       
       data.frame(
         Name = c("Error",
                  "Nivel de confianza",
                  "Personas encuestadas",
                  "Tamaño muestral",
                  "Tamaño poblacional"
                                    ),
         Value = as.character(c(input$bins1,
                                input$bins2,
                                rrr,
                                (tamao),
                                length(data1[, input$facultad])
                                
                            )
                            ),
         stringsAsFactors = FALSE)
       
     })
     # Generate a summary of the blmer  dataset
     output$summary2 <- renderTable({
       sliderValues()
              })
     
     
     
     sliderValues2 <- reactive({
       
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       #tabla de datos
       t2=ftable(data1[, input$facultad],row.vars = 1)
       t2=data.frame(t2)
       
       prop=round(t2[2]/sum(t2[2])*100,2)
       prop2=((t2[2]/sum(t2[2])))
       total=sum(t2[2])
       
       hh=as.numeric(((1-input$bins2)/2))
       z=qnorm(hh, mean = 0, sd = 1)
       
       #z=1.96
       total=sum(t2[2])
       prop2
       error=input$bins1
       q=(1-prop2)
       
       num=(total*prop2*q*(z^2))
       den=(error^2*(total-1))+(prop2*(1-prop2)*(z^2))
       tamao=round(num/den)
       
       
       ###Encuestados
       t2t=ftable(data1[, input$Programa],data1[, input$Encu],exclude=0,row.vars = 1:2)
       t2t=data.frame(t2t)
       
       vec=t2t$Freq
       ini=(length(vec)/2)+1
       
       vec2=vec[ini:length(vec)]
       rrr=sum(vec2)
       
        data.frame(
         Name = c("Error",
                  "Nivel de confianza",
                  "Personas encuestadas",
                  "Tamaño muestral",
                  "Tamaño poblacional"
         ),
         Value = as.character(c(input$bins1,
                                input$bins2,
                                rrr,
                                sum(tamao),
                                length(data1[, input$facultad])
                                
         )
         ),
         stringsAsFactors = FALSE)
       
     })
     output$summary4 <- renderTable({
       sliderValues2()
     })
     
     
     ### tabla dE TAMAÑOS MUESTRALES por programa y facultad
     output$contents26 <- renderDataTable({
       
       #lectura de datos
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       
       
       #tabla de datos
       t2=ftable(data1[, input$facultad],data1[, input$Programa],exclude=0,row.vars = 1:2)
       t2=data.frame(t2)
       #t2
       ##funcion para convertir 0 en NA
       haz.cero.na=function(x){
         ifelse(x==0,NA,x)}
       
       #pasos para quitar 0 de los datos
       dataf.2=data.frame(sapply(t2[3],haz.cero.na))
       tt=data.frame(t2,dataf.2)
       #tt
       ttt=na.omit(tt)
       #ttt
       tt1=ttt[-4]
       
       prop=round(ttt[3]/sum(ttt[3])*100,2)
       prop2=((tt1[3]/sum(tt1[3])))
       total=sum(tt1[3])
       
       hh=as.numeric(((1-input$bins2)/2))
       z=qnorm(hh, mean = 0, sd = 1)
       
       #z=1.96
       total=sum(ttt[3])
       prop2
       error=input$bins1
       q=(1-prop2)
       num=(total*prop2*q*(z^2))
       den=(error^2*(total-1))+(prop2*(1-prop2)*(z^2))
       tamao=sum(round(num/den))
       prop3=round(ttt[3]/sum(ttt[3]),2)
       tamal=round(prop3*tamao,0)
       
       t2t=ftable(data1[, input$Programa],data1[, input$Encu],exclude=0,row.vars = 1:2)
       t2t=data.frame(t2t)
   
       vec=t2t$Freq
       ini=(length(vec)/2)+1
       
       vec2=vec[ini:length(vec)]
       tt11=cbind(tt1,prop,tamal,vec2)
       colnames(tt11) <- c("Facultad","Programa","Conteo","Proporcion","Tamaño","Efectivas")
       tt11
       
     })
     
     
     output$cont26 <- renderDataTable({
       
       
       
       #lectura de datos
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       #tabla de datos
       t2=ftable(data1[, input$facultad],row.vars = 1)
       t2=data.frame(t2)
       
       
       prop2=((t2[2]/sum(t2[2])))
       total=sum(t2[2])
       
       hh=as.numeric(((1-input$bins2)/2))
       z=qnorm(hh, mean = 0, sd = 1)
       
       #z=1.96
       error=input$bins1
       q=(1-prop2)
       
       num=(sum(t2[2]*sqrt(prop2*q)))^2
       den1=sum((total*error/z)^2)
       den2=sum(t2[2]*prop2*q)
       
       taaa=num/(den1+den2)
       
       
       
       #tabla de datos
       t32=ftable(data1[, input$facultad],data1[, input$Programa],exclude=0,row.vars = 1:2)
       t32=data.frame(t32)
       #t2
       ##funcion para convertir 0 en NA
       haz.cero.na=function(x){
         ifelse(x==0,NA,x)}
       
       #pasos para quitar 0 de los datos
       dataf.2=data.frame(sapply(t32[3],haz.cero.na))
       tt=data.frame(t32,dataf.2)
       #tt
       ttt=na.omit(tt)
       #ttt
       tt1=ttt[-4]
       
       prop=round(ttt[3]/sum(ttt[3])*100,2)
       prop22=((tt1[3]/sum(tt1[3])))
      
      
       tamal=round(prop22*taaa)
       
       t2t=ftable(data1[, input$Programa],data1[, input$Encu],exclude=0,row.vars = 1:2)
       t2t=data.frame(t2t)
       
       vec=t2t$Freq
       ini=(length(vec)/2)+1
       
       vec2=vec[ini:length(vec)]
       tt11=cbind(tt1,prop,tamal,vec2)
       colnames(tt11) <- c("Facultad","Programa","Conteo","Proporcion","Tamaño","Efectivas")
       tt11
       
     })
     
     output$ex1 <- renderUI({
       withMathJax(helpText(h1(' $$  n = \\frac{ ( \\sum_{k=1}^{L} N_h \\sqrt{ p_h q_h} )^2}
                            { \\frac{N^2 B^2 }{K^2}+ \\sum_{k=1}^{L} N_h  p_h q_h} \  $$ ')))
     })
     
     
     
     ### tabla dE TAMAÑOS MUESTRALES por  facultad
     output$contents27 <- renderDataTable({
       
       #lectura de datos
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       #tabla de datos
       t2=ftable(data1[, input$facultad],row.vars = 1)
       t2=data.frame(t2)
      
       prop=round(t2[2]/sum(t2[2])*100,2)
       prop2=((t2[2]/sum(t2[2])))
       total=sum(t2[2])
       
       hh=as.numeric(((1-input$bins2)/2))
       z=qnorm(hh, mean = 0, sd = 1)
       
       #z=1.96
       total=sum(t2[2])
       prop2
       error=input$bins1
       q=(1-prop2)
       num=(total*prop2*q*(z^2))
       den=(error^2*(total-1))+(prop2*(1-prop2)*(z^2))
       tamao=round(num/den)
       
       ###Encuestados
       tefe=ftable(data1[, input$facultad],data1[, input$Encu],row.vars = 1:2)
       tefe=data.frame(tefe)
       
       op=(length(tefe$Freq)/2)+1
       tttsi=as.numeric(tefe$Freq[op:length(tefe$Freq)])
       
       tt11=cbind(t2,prop,tamao,tttsi)
       colnames(tt11) <- c("Facultad","Conteo","Proporcion","Tamao","Efectivas")
       tt11
       
     })
     
     ### tabla dE TAMAÑOS MUESTRALES por  facultad
     ##neyyyy
     output$tabb <- renderDataTable({
       
       #lectura de datos
       inFile <- input$file
       if (is.null(inFile))
         return(NULL)
       data1=read.table(inFile$datapath, header=T)
       
       #tabla de datos
       t2=ftable(data1[, input$facultad],row.vars = 1)
       t2=data.frame(t2)
       t2
       prop=round(t2[2]/sum(t2[2])*100,2)
       prop2=((t2[2]/sum(t2[2])))
       total=sum(t2[2])
       
       hh=as.numeric(((1-input$bins2)/2))
       z=qnorm(hh, mean = 0, sd = 1)
       
       #z=1.96
       error=input$bins1
       q=(1-prop2)
       
       num=(sum(t2[2]*sqrt(prop2*q)))^2
       den1=sum((total*error/z)^2)
       den2=sum(t2[2]*prop2*q)
       
       ttt=num/(den1+den2)
       
       
       ###Encuestados
       tefe=ftable(data1[, input$facultad],data1[, input$Encu],row.vars = 1:2)
       tefe=data.frame(tefe)
       
       op=(length(tefe$Freq)/2)+1
       tttsi=as.numeric(tefe$Freq[op:length(tefe$Freq)])
       
       tt11=cbind(t2,prop,round(ttt*prop2,0),tttsi)
       colnames(tt11) <- c("Facultad","Conteo","Proporcion","Tamaño","Efectivas")
       tt11
       
     })
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     # download report
     output$downloadReport <- downloadHandler(
       filename = function() {
         paste('my-report', sep = ".", switch(
           input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
         ))
       },
       
       content = function(file) {
         src <- normalizePath('report.Rmd')
         
         owd <- setwd(tempdir())
         on.exit(setwd(owd))
         file.copy(src, 'report.Rmd', overwrite = TRUE)
         
        out <- render('report.Rmd', switch(input$format,
           PDF = pdf_document(), HTML = html_document(), Word = word_document()
         ))
         
         file.rename(out, file)
       }
       )
     
     
     
     output$tabless15 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
    
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota),media=round(mean(nota,na.rm=TRUE),2),
                   SD=round(sd(nota,na.rm=TRUE),2),cv=round(sd(nota)/mean(nota)*100,2),
                   MIN=min(nota,na.rm=TRUE),MAX=max(nota,na.rm=TRUE))
                   
                   tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       tablas
     })
     
     output$tabless16 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota),media=round(mean(nota,na.rm=TRUE),2),
                   SD=round(sd(nota,na.rm=TRUE),2),cv=round(sd(nota)/mean(nota)*100,2),
                   MIN=min(nota,na.rm=TRUE),MAX=max(nota,na.rm=TRUE))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       tablas
     })
     
     
     output$tabless17 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota),media=round(mean(nota,na.rm=TRUE),2),
                   SD=round(sd(nota,na.rm=TRUE),2),cv=round(sd(nota)/mean(nota)*100,2),
                   MIN=min(nota,na.rm=TRUE),MAX=max(nota,na.rm=TRUE))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       tablas
     })
     
     output$tabless18 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota),media=round(mean(nota,na.rm=TRUE),2),
                   SD=round(sd(nota,na.rm=TRUE),2),cv=round(sd(nota)/mean(nota)*100,2),
                   MIN=min(nota,na.rm=TRUE),MAX=max(nota,na.rm=TRUE))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       tablas
     })
     output$pred_plot60<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       tobe2 <- which(clcdif$estado=="Reprobada") 
       DATA2 <- clcdif[tobe2,] 
       
       fdata=rbind.data.frame(DATA,DATA2)
       fdata=as.data.frame(fdata)
       
       g <- ggplot(fdata,aes(as.factor(fdata$periodo),as.numeric(fdata$nota)))
       g + geom_boxplot(fill = "white", colour = "#3366FF")+
         theme(axis.text.x = element_text(angle =0, hjust = 1,size=rel(1)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1)),
               axis.text = element_text(size = 16),legend.position = "left",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         facet_grid(~estado, scales="free", space="free")+
         labs(y= "Nota Promedio",x= "Período")
                   })
     
     
     output$pred_plot61<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       tobe2 <- which(clcdif$estado=="Reprobada") 
       DATA2 <- clcdif[tobe2,] 
       
       fdata=rbind.data.frame(DATA,DATA2)
       fdata=as.data.frame(fdata)
       
       g <- ggplot(fdata,aes(as.factor(fdata$periodo),as.numeric(fdata$nota)))
       g + geom_boxplot(fill = "white", colour = "#3366FF")+
         theme(axis.text.x = element_text(angle =0, hjust = 1,size=rel(1)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1)),
               axis.text = element_text(size = 16),legend.position = "left",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         facet_grid(~estado, scales="free", space="free")+
         labs(y= "Nota Promedio",x= "Período")
     })
     
     
     output$pred_plot62<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       tobe2 <- which(clcdif$estado=="Reprobada") 
       DATA2 <- clcdif[tobe2,] 
       
       fdata=rbind.data.frame(DATA,DATA2)
       fdata=as.data.frame(fdata)
       
       g <- ggplot(fdata,aes(as.factor(fdata$periodo),as.numeric(fdata$nota)))
       g + geom_boxplot(fill = "white", colour = "#3366FF")+
         theme(axis.text.x = element_text(angle =0, hjust = 1,size=rel(1)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1)),
               axis.text = element_text(size = 16),legend.position = "left",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         facet_grid(~estado, scales="free", space="free")+
         labs(y= "Nota Promedio",x= "Período")
     })
     
     output$pred_plot63<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       tobe2 <- which(clcdif$estado=="Reprobada") 
       DATA2 <- clcdif[tobe2,] 
       
       fdata=rbind.data.frame(DATA,DATA2)
       fdata=as.data.frame(fdata)
       
       g <- ggplot(fdata,aes(as.factor(fdata$periodo),as.numeric(fdata$nota)))
       g + geom_boxplot(fill = "white", colour = "#3366FF")+
         theme(axis.text.x = element_text(angle =0, hjust = 1,size=rel(1)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1)),
               axis.text = element_text(size = 16),legend.position = "left",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         facet_grid(~estado, scales="free", space="free")+
         labs(y= "Nota Promedio",x= "Período")
     })
     
     output$graf <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       
       tobes3 <- which(tablas$estado=="Aprobada")
       DATA3 <- tablas[tobes3,]
       
       ggplot(DATA3, aes(as.factor(DATA3$periodo),DATA3$prop,colour=DATA3$periodo)) +
         geom_col(fill=("skyblue"))+
         labs(x= "Período",y= "Proporción de estudiantes aprobados",color="Periodo")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.01),labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.4)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1.4)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1.4)),
               legend.title=element_text(size = rel(1)),
               axis.text = element_text(size = 14),legend.position = "bottom",
               strip.text.x = element_text(size=18),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         coord_cartesian(ylim = c(min(DATA3$prop), max(DATA3$prop)))
       
     })
     
     
     output$graf7 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       
       tobes3 <- which(tablas$estado=="Aprobada")
       DATA3 <- tablas[tobes3,]
       
       ggplot(DATA3, aes(as.factor(DATA3$periodo),DATA3$prop,colour=DATA3$periodo)) +
         geom_col(fill=("skyblue"))+
         labs(x= "Período",y= "Proporción de estudiantes aprobados",color="Periodo")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.01),labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.4)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1.4)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1.4)),
               legend.title=element_text(size = rel(1)),
               axis.text = element_text(size = 14),legend.position = "bottom",
               strip.text.x = element_text(size=18),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         coord_cartesian(ylim = c(min(DATA3$prop), max(DATA3$prop)))
       
     })
     
     
     output$graf14 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       
       tobes3 <- which(tablas$estado=="Aprobada")
       DATA3 <- tablas[tobes3,]
       
       ggplot(DATA3, aes(as.factor(DATA3$periodo),DATA3$prop,colour=DATA3$periodo)) +
         geom_col(fill=("skyblue"))+
         labs(x= "Período",y= "Proporción de estudiantes aprobados",color="Periodo")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.01),labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.4)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1.4)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1.4)),
               legend.title=element_text(size = rel(1)),
               axis.text = element_text(size = 14),legend.position = "bottom",
               strip.text.x = element_text(size=18),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         coord_cartesian(ylim = c(min(DATA3$prop), max(DATA3$prop)))
       
     })
     
     
     output$graf21 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       tablas=cbind(tabla,prop)
       
       tobes3 <- which(tablas$estado=="Aprobada")
       DATA3 <- tablas[tobes3,]
       
       ggplot(DATA3, aes(as.factor(DATA3$periodo),DATA3$prop,colour=DATA3$periodo)) +
         geom_col(fill=("skyblue"))+
         labs(x= "Período",y= "Proporción de estudiantes aprobados",color="Periodo")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.01),labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.4)), 
               axis.text.y = element_text(hjust = 1,size=rel(1)),
               axis.title.x = element_text(size = rel(1.4)),
               axis.title.y = element_text(size = rel(1)),
               legend.text = element_text(size = rel(1.4)),
               legend.title=element_text(size = rel(1)),
               axis.text = element_text(size = 14),legend.position = "bottom",
               strip.text.x = element_text(size=18),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         coord_cartesian(ylim = c(min(DATA3$prop), max(DATA3$prop)))
       
     })
     
     output$graf8 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=10,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue" )
       
       
     })
     
     output$graf15 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=10,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue" )
       
       
     })
     
     output$graf22<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=10,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue" )
       
       
     })
     
     
     output$graf1 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=10,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue" )
       
       
     })
     
     
     
     output$graf2 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- datos[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
            })
     
     
     
     output$graf9 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
     })
     
     
     output$graf16<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
     })
     
     output$graf23<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
     })
     
     
     output$tabb2 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- datos[tobes,]
      
         
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       
       per1=des2[[3]]
       
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 10)
       y1=table.freq(tmp2)
       
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       
       
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 10)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks =10)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
           })
     
     
     
     
     output$tabb6 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       
       per1=des2[[3]]
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 10)
       y1=table.freq(tmp2)
       
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 10)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks =10)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     
     
     output$tabb10<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       
       per1=des2[[3]]
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 10)
       y1=table.freq(tmp2)
       
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 10)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks =10)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     
     
     
     output$tabb14<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(estado=="Aprobada")
       DATA <- clcdif[tobes,]
       
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       
       per1=des2[[3]]
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 10)
       y1=table.freq(tmp2)
       
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 10)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks =10)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     output$graf3 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=6,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue",ylim =c(5,25) )
       
       
            })
     
     
     output$graf10 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=6,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue",ylim =c(8,28) )
       
       
     })
     
  
     
     
     output$graf17 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=6,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue",ylim =c(0,45) )
       
       
     })
     
     output$graf24 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       histogram(~ DATA$nota | factor(DATA$periodo),breaks=6,layout = c(3,1),type=c("percent"),
                 data = DATA, plot.points = FALSE, auto.key = TRUE,right = FALSE,
                 xlab = "Puntaje Matemáticas",
                 ylab = "Densidad",col="skyblue",ylim =c(10,25) )
       
       
     })
     
     output$graf4 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(datos$estado=="Reprobada")
       DATA <- datos[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
       
       
     })
     
     
     
     output$graf11 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
       
       
     })
     
     
     output$graf18 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
                   })
     
     
     output$graf25 <- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(clcdif$estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       densityplot(~ DATA$nota, data = DATA, groups = DATA$periodo,
                   plot.points = FALSE, auto.key = TRUE, 
                   xlab = "Densidad de las notas aprobadas",
                   ylab = "Densidad")
     })
     
     
     output$tabb7 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tobes <- which(estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       per1=des2[[3]]
       
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y1=table.freq(tmp2)
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       
       
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 6)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     
     
     
     output$tabb11 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tobes <- which(estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       per1=des2[[3]]
       
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y1=table.freq(tmp2)
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       
       
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 6)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     
     
     
     output$tabb15 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tobes <- which(estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       per1=des2[[3]]
       
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y1=table.freq(tmp2)
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       
       
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 6)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     output$tabb3 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tobes <- which(estado=="Reprobada")
       DATA <- clcdif[tobes,]
       
       matriz=data.frame(DATA$periodo,DATA$nota)
       des2=split(matriz,DATA$periodo)
       per1=des2[[3]]
       
       tmp2 <- hist(as.numeric(per1$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y1=table.freq(tmp2)
       per20181=data.frame(y1$Lower,y1$Upper,y1$Frequency,y1$Percentage,y1$CPF)
       
       
       per2=des2[[2]]
       tmp2 <- hist(as.numeric(per2$DATA.nota),plot=FALSE, right = FALSE,breaks = 6)
       y2=table.freq(tmp2)
       per20=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,y2$Percentage,y2$CPF)
       
       
       per3=des2[[1]]
       tmp2 <- hist(as.numeric(per3$DATA.nota),plot=FALSE,right = FALSE,breaks = 6)
       y3=table.freq(tmp2)
       completa=data.frame(y1$Lower,y1$Upper,y1$Percentage,y1$CPF,
                           y2$Percentage,y2$CPF,y3$Percentage,y3$CPF)
       colnames(completa) <- c("Lim inf","Lim sup","% 2018-1","% Acum 2018-1","% 2017-2","% Acum 2017-2","% 2017-1","% Acum 2017-1")
       
       completa
     })
     
     
     output$graf5<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
  
       tablas=cbind(tabla,prop)
       
       tobes3 <- which(tablas$estado=="Aprobada")
       DATA3 <- tablas[tobes3,]
       DATA3<-DATA3[order(as.factor(DATA3$facultad)),]
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$facultad,colour=DATA3$facultad))+
         geom_line(lwd=1)+scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.02),
                                             labels = scales::percent)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.55)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     
     
     output$graf12<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
      
       
       prop=round((DATA3$n/tots$`length(nota)`),2)
       tablas=cbind(DATA3,prop)
       
        DATA3=tablas
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$facultad,colour=DATA3$facultad))+
         geom_line(lwd=1)+scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.02),
                                             labels = scales::percent)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.55)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     
     output$graf19<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       
       
       prop=round((DATA3$n/tots$`length(nota)`),2)
       tablas=cbind(DATA3,prop)
       
       DATA3=tablas
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$facultad,colour=DATA3$facultad))+
         geom_line(lwd=1)+scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.02),
                                             labels = scales::percent)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.55)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     
     output$graf26<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       
       
       prop=round((DATA3$n/tots$`length(nota)`),2)
       tablas=cbind(DATA3,prop)
       
       DATA3=tablas
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$facultad,colour=DATA3$facultad))+
         geom_line(lwd=1)+scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.02),
                                             labels = scales::percent)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.55)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     output$tabb4 <- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       vec=rep(valor,each=4)
       prop=round((tabla$n/vec),2)
       
       
       tablas=cbind(tabla,prop)
       
       tobes3 <- which(tablas$estado=="Aprobada")
       DATA3 <- tablas[tobes3,]
       DATA3<-DATA3[order(as.factor(DATA3$facultad)),]
      
            })
     
     
     output$tabb8<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       prop=round((DATA3$n/tots$`length(nota)`),2)
       tablas=cbind(DATA3,prop)
       tablas
            })
     
     
     output$tabb12<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       prop=round((DATA3$n/tots$`length(nota)`),2)
       tablas=cbind(DATA3,prop)
       tablas
     })
     
     
     
     
     output$tabb16<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,facultad,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       tots=clcdif %>%
         group_by(periodo,facultad)%>%
         summarise(length(nota))
       
       prop=round((DATA3$n/tots$`length(nota)`),2)
       tablas=cbind(DATA3,prop)
       tablas
     })
     output$graf6<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
      
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
      
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$sede,colour=DATA3$sede))+
         geom_line(lwd=1)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.05),
                             labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     output$graf13<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$sede,colour=DATA3$sede))+
         geom_line(lwd=1)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.05),
                            labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     
     
     
     
     output$graf13<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$sede,colour=DATA3$sede))+
         geom_line(lwd=1)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.05),
                            labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     
     output$graf20<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$sede,colour=DATA3$sede))+
         geom_line(lwd=1)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.05),
                            labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     
     
     
     output$graf27<- renderPlot({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       
       
       ggplot(DATA3, aes(x=as.factor(DATA3$periodo),y=as.numeric(DATA3$prop),
                         group=DATA3$sede,colour=DATA3$sede))+
         geom_line(lwd=1)+
         labs(x="Período" ,y= "Proporción",color="Facultad")+
         scale_y_continuous(breaks=seq(min(DATA3$prop),max(DATA3$prop),0.05),
                            labels = scales::percent)+
         theme(axis.text.x = element_text(angle = 0, hjust = 1,size=rel(1.5)), 
               axis.text.y = element_text(hjust = 1,size=rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title=element_text(size = rel(1.5)),
               axis.text = element_text(size = 12),legend.position = "bottom",
               strip.text.x = element_text(size=16),
               panel.background = element_rect(fill = "grey95", colour = "grey50"),
               strip.background = element_rect(colour = "black", fill = "grey99")) +
         scale_x_discrete(limits=levels(DATA3$periodo))
       
       
     })
     output$tabb9<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[2]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       DATA3
       
      
       
       
     })
     output$tabb13<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[3]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       DATA3            })
     
     output$tabb17<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[4]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       
       DATA3
       
       
       
       
     })
     
     
     output$tabb5<- renderDataTable({
       
       datos <- read.delim("Libro1.txt",header=T,dec=",",sep = "\t")
       
       attach(datos)
       mate=split(datos,datos$materia )
       clcdif=mate[[1]]
       attach(clcdif)
       
       tabla=clcdif %>%
         group_by(periodo,sede,estado)%>%
         summarise(n=length(nota))
       
       tabla=as.data.frame(tabla)
       
       tobes3 <- which(tabla$estado=="Aprobada")
       DATA3 <- tabla[tobes3,]
       
       
       tots=clcdif %>%
         group_by(periodo,sede)%>%
         summarise(length(nota))
       
       valor=as.vector(tots$`length(nota)`)
       prop=round((DATA3$n/valor),2)
       DATA3=cbind(DATA3,prop)
       DATA3
       
     })
     
     
     
})