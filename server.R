
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(readxl)
library(ggplot2)
library(agricolae)
library(car)
library(knitr)
library(lattice)

mydata = c("Columna..." = "")

shinyServer(function(input, output) {
  
  
  Rend<-reactive({inFile <- input$file1
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  Rend<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })
  
  output$select_data <- renderUI({
    if (is.null(Rend())){
      mydata = c("Columna..." = "")
      print(paste0("You have chosen: ", Rend()[1,]))
    }else{
     
      print(paste0("You have chosen: ", Rend()[1,]))
    }  
    
    selectInput ("choose_data", 
                label = "Tratamientos",
                choices = mydata)
  })
  
  output$contents <- renderDataTable({
   if (is.null(Rend())){
     Datos<-c("No hay datos cargados!! Carga un archivo xlsx. Los Entrys deben tener una columna de TRATAMIENTO, BLOQUE y VARIABLE DEPENDIENTE. Como Opcional se puede carga la Col y Row del entry en el plano del ensayo")
     
     d<-data.table(Datos)
     datatable(d,rownames = FALSE, colnames = NULL, options = list(dom = 't'))
   }
    else{
    tab1<-Rend()
   tab1[,input$n1]<-lapply(tab1[,input$n1], factor)
   tab1[,input$n2]<-lapply(tab1[,input$n2], factor)
   datatable(tab1, rownames = FALSE,options = list(pageLength = 15)) %>% formatStyle(
     names(tab1[input$n1]), backgroundColor = '#6393fd')%>% formatStyle(
       names(tab1[input$n2]),backgroundColor = '#91b3fe')%>% formatStyle(
       names(tab1[input$n3]),backgroundColor = "#ceddfe")%>% formatString(
      input$n1)%>% formatString(
        input$n2)%>% formatRound(1:10,input$n4 ) 
  }} )
  
  output$plano <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    if (input$n8==0||input$n9==0){
      return(NULL)}
    else{
      dat<-Rend()
      setnames(dat, input$n8, "row")
      setnames(dat, input$n9, "col")
      setnames(dat, input$n3, "VarDep")
      levelplot(VarDep ~ col*row, data=dat,xlab = "Columnas",ylab = "Filas",sub="Cada rectangulo representa una parcela", main="Heat Map de la Var. Dep en el plano del ensayo")
      
    }})
  
  output$plots <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    tab<-Rend()
    tab[,input$n1]<-lapply(tab[,input$n1], factor)
    tab[,input$n2]<-lapply(tab[,input$n2], factor)
    setnames(tab, input$n1, "Tratamiento")
    setnames(tab, input$n2, "Bloque")
    setnames(tab, input$n3, "RS")
    ggplot(tab, aes(x=Tratamiento ,y=RS))+geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 4)+xlab("Tratamiento") + ylab(input$n5)+geom_jitter(aes(colour = Bloque),width = 0 )+theme_bw()+theme(axis.text.x = element_text(angle=90))
    }})
  
  
  output$plotsblq <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    tab<-Rend()
    tab[,input$n1]<-lapply(tab[,input$n1], factor)
    tab[,input$n2]<-lapply(tab[,input$n2], factor)
    names(tab[input$n1])<-"Tratamiento"
    setnames(tab, input$n2, "Bloque")
    setnames(tab, input$n3, "RS")
    labe<-input$n5
  ggplot(tab, aes(x=Bloque , y=RS , group = Tratamiento, color = Tratamiento)) + geom_point(data = tab, aes(y = RS)) + geom_line(data = tab, aes(y = RS, group = Tratamiento, color=Tratamiento))+ xlab("Bloque") +ylab(input$n5)+  theme_bw()+ theme(panel.grid.minor = element_blank(),legend.position="bottom")
  }})
  
  
  
  output$table <- renderDataTable({
    if (is.null(Rend())){
      Datos<-c("No hay datos cargados!! Carga un archivo xlsx en la pestaña de Datos")
      
      data.table(Datos)
      
      }
    else{
    tab<-Rend()
    tab<-data.table(tab)
    
    setnames(tab, input$n1, "Tratamiento")
    setnames(tab, input$n2, "Bloque")
    setnames(tab, input$n3, "VarDep")
    
    tra=colnames(Rend()[,input$n1])
    tab2<-tab[ , lapply(.SD, mean), by=c("Tratamiento"), .SDcols =-input$n2]
    d1<-tab2[which(tab$Tratamiento==input$n7)]
    tab2$DifTest<-d1[1,c("VarDep")]
    tab2$DifTest<-tab2$VarDep-tab2$DifTest
    datatable(tab2,rownames = FALSE, options = list(
      dom = 't'))%>% formatRound(1:10,input$n4 ) 
  }})
  
  output$anova <- renderPrint({
    if (is.null(Rend())){
      return( )}
    else{
    dat<-Rend()
    names(dat[input$n1])<-"Tratamiento"
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    summary(ANOVA.Rend)
  }})
  
  output$LSD <- renderPrint({
    if (is.null(Rend())){
      return( )}
    else{
    dat<-Rend()
    
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    
    LSD.Trat.Rend<-LSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, console = T)
  }})
  
  output$plotlsd <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    mx<-max(dat[ , input$n3])
    mx1<-mx/10
    mx2<-mx+mx1
    LSD.Trat.Rend<-LSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, console = F)
    bar.group(x = LSD.Trat.Rend$groups, 
            ylim=c(0,mx2),
            lwd=2,
            main="Prueba de comparación de medias - LSD",
            xlab="Tratamiento",
            ylab=input$n5,
            las=3,
            col="#6393fd")
  }})
  
  output$tabla <- renderDataTable({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    datatable(dat,rownames = FALSE,options = list(pageLength = 15))%>% formatString(
      input$n2)%>% formatString(
        input$n1)%>% formatRound(1:10,input$n4 ) 
  }})
  
  output$disres <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
  qqPlot(rstandard(ANOVA.Rend), main="Normal Q-Q " )
 
  }})
  
  output$hista <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      dat<-Rend()
      
      setnames(dat, input$n1, "Tratamiento")
      setnames(dat, input$n2, "Bloque")
      setnames(dat, input$n3, "VarDep")
      dat[,input$n1]<-lapply(dat[,input$n1], factor)
      dat[,input$n2]<-lapply(dat[,input$n2], factor)
      modelo.Rend<-lm(VarDep ~ Tratamiento,data=dat)
      ANOVA.Rend<-aov(modelo.Rend)
      Residuales<-ANOVA.Rend$residuals
      h<-hist(Residuales, 
              col="darkgray")
      xfit<-seq(min(Residuales),max(Residuales),length=40) 
      yfit<-dnorm(xfit,mean=mean(Residuales),sd=sd(Residuales)) 
      yfit <- yfit*diff(h$mids[1:2])*length(Residuales) 
      lines(xfit, yfit, col="blue", lwd=2) 
      
    }})
  
  output$shap <- renderPrint({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
  shapiro.test(ANOVA.Rend$residuals)
  }})
  
  output$homo <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    
    ggplot(dat,aes(x=Predichos, y=Residuos))+geom_point(main="Predichos vs Residuos ")+theme_bw()
   
    }})
  
  output$test <- renderPrint({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    bartlett.test(Residuos ~ Tratamiento,data=dat)
     
  }})
  
  output$resi <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      dat<-Rend()
      setnames(dat, input$n1, "Tratamiento")
      setnames(dat, input$n2, "Bloque")
      setnames(dat, input$n3, "VarDep")
      dat[,input$n1]<-lapply(dat[,input$n1], factor)
      dat[,input$n2]<-lapply(dat[,input$n2], factor)
      modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
      ANOVA.Rend<-aov(modelo.Rend)
      Predichos<-fitted(ANOVA.Rend)#creamos los predichos
      Residuos<-residuals(ANOVA.Rend)
      dat$Predichos<-Predichos
      dat$Residuos<-Residuos
      boxplot(Residuos~ Tratamiento,data=dat, las=3 ) 
    }})
  
  output$aditi <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat$Bloque<-as.character(dat$Bloque)
  ggplot(dat, aes(x=Bloque , y=VarDep , group = Tratamiento, color = Tratamiento)) + geom_point(data = dat, aes(y = VarDep)) + geom_line(data = dat, aes(y = VarDep, group = Tratamiento, color=Tratamiento))+ xlab("Bloque") +ylab(input$n5)+  theme_bw()
  }})
  
  output$inde <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      dat<-Rend()
      
      setnames(dat, input$n1, "Tratamiento")
      setnames(dat, input$n2, "Bloque")
      setnames(dat, input$n3, "VarDep")
      dat[,input$n1]<-lapply(dat[,input$n1], factor)
      dat[,input$n2]<-lapply(dat[,input$n2], factor)
      modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
      ANOVA.Rend<-aov(modelo.Rend)
      plot(ANOVA.Rend$residuals)
    }})
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(tab = Rend(), i1=input$n1, i2=input$n2, i3=input$n3, i4=input$n4, i5=input$n5, i7=input$n7, i6=input$n6 )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})

