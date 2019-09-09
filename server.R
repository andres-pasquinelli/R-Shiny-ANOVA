
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


mydata = c("Columna..."= "")

shinyServer(function(input, output) {
  
 
  Rend<-reactive({inFile <- input$file1
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  Rend<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })
  
  Rend2<-reactive({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    Rend<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    
  })
  Rend3<-reactive({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    tab<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    tab[input$choose_tratamiento]<-lapply(tab[input$choose_tratamiento], factor)
    tab[input$choose_bloque]<-lapply(tab[input$choose_bloque], factor)
    setnames(tab, input$choose_tratamiento, "Tratamiento")
    setnames(tab, input$choose_bloque, "Bloque")
    setnames(tab, input$choose_dependiente, "VarDep")
  })
  Rend4<-reactive({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    dat<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    setnames(dat, input$choose_row, "row")
    setnames(dat, input$choose_col, "col")
    setnames(dat, input$choose_dependiente, "VarDep")
  })
  
    
  
  
  
  
  output$tratamientos <- renderUI({
    if (is.null(Rend())){
      mydata = c("Columna..." = "")
      
    }else{
     
      mydata <-c(mydata,names(Rend()))
    }  
    
    selectInput ("choose_tratamiento", 
                 label = NULL,
                choices = mydata)
  })
  output$bloques <- renderUI({
    if (is.null(Rend())){
      mydata = c("Columna..."= "")
      
    }else{
      
      mydata <-c(mydata,names(Rend()))
    }  
    
    selectInput ("choose_bloque", 
                 label = NULL,
                 choices = mydata)
  })
  output$dependiente <- renderUI({
    if (is.null(Rend())){
      mydata = c("Columna..." = "")
      
    }else{
      
      mydata <-c(mydata,names(Rend()))
    }  
    
    selectInput ("choose_dependiente", 
                 label = NULL,
                 choices = mydata)
  })
  output$row <- renderUI({
    if (is.null(Rend())){
      mydata = c("Columna..." = "")
      
    }else{
      
      mydata <-c(mydata,names(Rend()))
    }  
    
    selectInput ("choose_row", 
                 label = "Row",
                 choices = mydata)
  })
  output$compMultDesc <- renderUI({
    if(input$compMult==""){
      "Selecciona la Prueba de comparación múltiple de medias deseada"
    }else if(input$compMult=="LSD"){
      'Método de la diferencia mínima significativa, Least Significant Difference (LSD). 
      El Test se basa en la creación de un valor común, un umbral, basado en un test de la t de Student. Se realizan todas las diferencias entre medias de los t niveles. Las diferencias que estén por encima de este umbral indicarán una diferencia de medias significativa y las diferencias que estén por debajo indicarán una diferencia no significativa'
    }else if(input$compMult=="Tukey"){
      'Test HSD (Honestly-significant-difference) de Tukey. Se basa en la distribución del rango estudentizado que es la distribución que sigue la diferencia del máximo y del mínimo de las diferencias entre la media muestral y la media poblacional de t variables normales N(0, 1) independientes e idénticamente distribuidas.
Se establece así un umbral, como en otros métodos, como el Test LSD. Se calculan todas las diferencias de medias muestrales entre los t niveles del factor estudiado. Las diferencias que estén por encima de ese umbral se considerarán diferencias significativas, las que no lo estén se considerarán diferencias no significativas.'
    }else if(input$compMult=="SNK"){
      'Método de  Student-Newman-Keuls (SNK). Este Test es realmente paralelo al Test de Duncan. Utiliza un umbral móvil, como esa técnica, basado en el número de medias que están implicadas en el recorrido de la resta de medias comparada pero con una diferencia: aquí el nivel de significación no cambia, no se altera, se mantiene en el general, que suele ser, como siempre en Estadística, 0.05. No aumenta como sucede en el Test de Duncan. Esto le convierte en un Test más conservador, con menos potencia.'
    }else if(input$compMult=="Scheffé"){
      'El Test de Scheffé crea también umbral, como las otras técnicas de comparaciones múltiples, y las diferencias que superen ese umbral serán, para el método, significativas, y las que no lo superen no lo serán.

'
    }else if(input$compMult=="Duncan"){
      'El Test de Duncan es muy similar al Test HSD de Tukey, pero en lugar de trabajar con un umbral fijo trabaja con un umbral cambiante. Un umbral que dependerá del número de medias implicadas en la comparación.

Para saber el número de medias implicadas en la comparación se ordenan las medias muestrales de menor a mayor y así al hacer una comparación entre dos medias sabremos además de las dos medias comparadas cuantas medias quedan dentro. Este número de medias implicadas en cualquier comparación de medias es el parámetro p de este umbral.'
    }else if(input$compMult=="Bonferroni"){
      'El Test de Bonferroni se basa en la creación de un umbral, el BSD (Bonferroni significant difference) por encima del cual, como el LSD en el Test LSD, la diferencia entre las dos medias será significativa y por debajo del cual esa diferencia no lo será de estadísticamente significativa.'
    }
    
  })
  output$col <- renderUI({
    if (is.null(Rend())){
      mydata = c("Columna..." = "")
      
    }else{
      
      mydata <-c(mydata,names(Rend()))
    }  
    
    selectInput ("choose_col", 
                 label = "Col",
                 choices = mydata)
  })
  
  output$contents <- renderDataTable({
   if (is.null(Rend2())){
     Datos<-c("No hay datos cargados!! Carga un archivo xlsx. Los Entrys deben tener una columna de TRATAMIENTO, BLOQUE y VARIABLE DEPENDIENTE. Como Opcional se puede carga la Col y Row del entry en el plano del ensayo")
     
     d<-data.table(Datos)
     datatable(d,rownames = FALSE, colnames = NULL, options = list(dom = 't'))
   }else{
     
      tab1<-Rend2()
   
   if(identical(input$choose_tratamiento, "")){
     tratamiento=1
     tratamientoColor="#ffffff"
   }else{
     tratamiento=input$choose_tratamiento
     tratamientoColor="#ceddfe"
     tab1[input$choose_tratamiento]<-lapply(tab1[input$choose_tratamiento], factor)
   }
   
   if(identical(input$choose_bloque, "")){
     bloque=1
     bloqueColor="#ffffff"
   }else{
     bloque=input$choose_bloque
     bloqueColor="#91b3fe"
     tab1[input$choose_bloque]<-lapply(tab1[input$choose_bloque], factor)
   }
   if(identical(input$choose_dependiente, "")){
     dep=1
     depColor="#ffffff"
   }else{
     dep=input$choose_dependiente
     depColor="#6393fd"
   }
  
   datatable(tab1, rownames = FALSE,options = list(pageLength = 15)) %>% formatStyle(
     names(tab1[tratamiento]), backgroundColor = tratamientoColor)%>% formatStyle(
       names(tab1[bloque]),backgroundColor = bloqueColor)%>% formatStyle(
       names(tab1[dep]),backgroundColor = depColor)%>% formatString(
      input$n1)%>% formatString(
        input$n2)%>% formatRound(1:10,input$n4 )} 
   
  })
  
  output$plano <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    if (identical(input$choose_row, "")||identical(input$choose_col,"")){
      return(NULL)}
    else{
      dat<-Rend4()
      levelplot(VarDep ~ col*row, data=dat,xlab = "Columnas",ylab = "Filas",sub="Cada rectangulo representa una parcela", main="Heat Map de la Var. Dep en el plano del ensayo")
      
    }})
  
  output$plots <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    tab<-Rend3()
    ggplot(tab, aes(x=Tratamiento ,y=VarDep))+geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 4)+xlab("Tratamiento") + ylab(input$n5)+geom_jitter(aes(colour = Bloque),width = 0 )+theme_bw()+theme(axis.text.x = element_text(angle=90))
    }})
  
  
  output$plotsblq <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      tab<-Rend3()
    labe<-input$n5
  ggplot(tab, aes(x=Bloque , y=VarDep , group = Tratamiento, color = Tratamiento)) + geom_point(data = tab, aes(y = VarDep)) + geom_line(data = tab, aes(y = VarDep, group = Tratamiento, color=Tratamiento))+ xlab("Bloque") +ylab(input$n5)+  theme_bw()+ theme(panel.grid.minor = element_blank(),legend.position="bottom")
  }})
  
  
  
  output$table <- renderDataTable({
    if (is.null(Rend())){
      Datos<-c("No hay datos cargados!! Carga un archivo xlsx en la pestaña de Datos")
      
      data.table(Datos)
      
    }
    
    else{
      
    tabs<-Rend3()
    
    
    
    tabs<-data.table(tabs)
    changeCols <- colnames(tabs)[which(as.vector(tabs[,lapply(.SD, class)]) == "character")]
    tabs[,(changeCols):= NULL]
    
    tab2<-tabs[ , lapply(.SD, mean), by=c("Tratamiento"), .SDcols =-"Bloque"]
    d1<-tab2[which(tabs$Tratamiento==input$n7)]
    tab2$DifTest<-d1[1,c("VarDep")]
    tab2$DifTest<-tab2$VarDep-tab2$DifTest
    datatable(tab2,rownames = FALSE, options = list(
      dom = 't'))%>% formatRound(1:10,input$n4 ) 
  }})
  
  output$anova <- renderPrint({
    if (is.null(Rend())){
      return( )}
    else{
    dat<-Rend3()
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    summary(ANOVA.Rend)
  }})
  output$compMultTabla <- renderPrint({
    if (is.null(Rend())){
      return( )}
    dat<-Rend3()
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    if (input$compMult==""){
      return( )}
    else if(input$compMult=='LSD'){
      LSD.Trat.Rend<-LSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, console = T)
    }else if(input$compMult=='Tukey'){
      outHSD<-HSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6 ,console=TRUE)
    }else if(input$compMult=='Duncan'){
      duncan.test(ANOVA.Rend, "Tratamiento", alpha = input$n6,console=TRUE)
    }else if (input$compMult=='SNK'){
      SNK.test(ANOVA.Rend, "Tratamiento", alpha = input$n6,console=TRUE)
    }else if (input$compMult=='Scheffé'){
      scheffe.test(ANOVA.Rend, "Tratamiento", alpha = input$n6,console=TRUE)
    }else if (input$compMult=='Bonferroni'){
      LSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, p.adj= "bon",console=TRUE)
    }
    })
  output$compMultPlot <- renderPlot({
    if (is.null(Rend())){
      return( )}
    dat<-Rend3()
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    mx<-max(dat["VarDep"])
    mx1<-mx/10
    mx2<-mx+mx1
    
    if (input$compMult==""){
      return(NULL)}
    else if(input$compMult=='LSD'){
      Trat.Rend<-LSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, console = F)
    }else if(input$compMult=='Tukey'){
      Trat.Rend<-HSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, console = F)
    }else if(input$compMult=='Duncan'){
      Trat.Rend<-duncan.test(ANOVA.Rend, "Tratamiento", alpha = input$n6,console=TRUE)
    }else if(input$compMult=='SNK'){
      Trat.Rend<-SNK.test(ANOVA.Rend, "Tratamiento", alpha = input$n6,console=TRUE)
    }else if(input$compMult=='Scheffé'){
      Trat.Rend <-scheffe.test(ANOVA.Rend, "Tratamiento", alpha = input$n6,console=TRUE)
    }else if(input$compMult=='Bonferroni'){
      Trat.Rend <-LSD.test(ANOVA.Rend, "Tratamiento", alpha = input$n6, p.adj= "bon",console=TRUE)
    }
    bar.group(x = Trat.Rend$groups, 
              ylim=c(0,mx2),
              lwd=2,
              main=paste('Prueba de comparación de medias -',input$compMult),
              xlab="Tratamiento",
              ylab=input$n5,
              las=3,
              col="#6393fd")
    })
  output$cv <- renderPrint({
    if (is.null(Rend())){
      return( )}
    else{
      dat<-Rend3()
      modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
      ANOVA.Rend<-aov(modelo.Rend)
      cv.model(ANOVA.Rend)
    }})
  
  
  output$tabla <- renderDataTable({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend3()
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    datatable(dat,rownames = FALSE,options = list(pageLength = 15))%>% formatString(
      "Bloque")%>% formatString(
        "Tratamiento")%>% formatRound(1:10,input$n4 ) 
  }})
  
  output$disres <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend3()
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
  qqPlot(rstandard(ANOVA.Rend), main="Normal Q-Q " )
 
  }})
  
  output$hista <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      dat<-Rend3()
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
    dat<-Rend3()
    modelo.Rend<-lm(VarDep ~ Tratamiento+Bloque,data=dat)
    ANOVA.Rend<-aov(modelo.Rend)
  shapiro.test(ANOVA.Rend$residuals)
  }})
  
  output$homo <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend3()
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
    dat<-Rend3()
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
      dat<-Rend3()
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
    dat<-Rend3()
  ggplot(dat, aes(x=Bloque , y=VarDep , group = Tratamiento, color = Tratamiento)) + geom_point(data = dat, aes(y = VarDep)) + geom_line(data = dat, aes(y = VarDep, group = Tratamiento, color=Tratamiento))+ xlab("Bloque") +ylab(input$n5)+  theme_bw()
  }})
  
  output$inde <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      dat<-Rend3()
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

