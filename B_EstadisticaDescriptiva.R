library(rhandsontable)
library(readr)
library(rpivotTable)

#Estadistica descriptiva
renderBaseTableOption <- function(id = "leerArchivo", label = "leerArchivo") {
  ns <- NS(id)
  tagList(
    selectInput(ns("mode"),"Cargar desde:" ,c("Archivo", "Llenado de Tabla")),
    conditionalPanel (
      condition = "input['leerArchivo-mode'] == 'Archivo'",
      uiOutput(ns("renderReadFileoptions"))  
    ),
    conditionalPanel (
      condition = "input['leerArchivo-mode'] == 'Llenado de Tabla'",
      uiOutput(ns("renderInputTableoptions"))
    )
  )
}

renderTableUI <- function(id = "leerArchivo", label = "leerArchivo") {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Tabla",
      conditionalPanel (
        condition = "input['leerArchivo-mode'] == 'Archivo'",
        tags$div(style='padding-top:20px;', 
           dataTableOutput(ns("tableUI")))
      ),
      conditionalPanel (
        condition = "input['leerArchivo-mode'] == 'Llenado de Tabla'",
        tagList(h4("Captura los campos y escribe los valores"),rHandsontableOutput(ns("inputTableUI")))
      )       
    ),
    #tabPanel("Tabla Dinámica", rpivotTableOutput(ns("pivoteTable"))),
    tabPanel("Visualización de Datos", 
             mainPanel( selectInput(ns("tipoGrafica"), "Tipo de gráfico", 
                    c("Histograma","Barras","Pastel","Ojiva","Boxplot", "Tallo y Hojas", "Gráfico De puntos", "Dispersión")),
                    uiOutput(ns("visualizacion")),
                    downloadButton(ns('guardarExport'), 'Exportar Imagen'),
                    h4(sprintf("Código de la Gráfica")),
                    uiOutput(ns('codigo'))
                    )
             )
  )
}

setupDescriptivaListeners <- function(input, output, session, label = "leerArchivo") {
  ns <- session$ns
  tableValues <- reactiveValues(table = NULL)
  
  output$renderReadFileoptions <- renderUI({
    verticalLayout(
      # Input: Select a file ----
      fileInput( ns("file") , "Selecciona el archivo", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values","text/plain", ".csv", ".txt")),
      
      # Input: Checkbox if file has header ----
      checkboxInput(ns("header"), "Encabezados", TRUE),
      
      # Input: Checkbox if file has header ----
      checkboxInput(ns("rownames"), "Remover Columna Indices", FALSE),
      
      # Input: Select separator ----
      radioButtons( ns("sep"), "Separador", choices = c(Coma = ",", "Punto y coma" = ";", "Tabulación" = "\t", Pipe = "|"), selected = ","),
      
      # Input: Select quotes ----
      radioButtons( ns("quote"), "Comillas", choices = c("Ninguna" = "", "Simple" = "'", "Doble" = '"'), selected = '"')
    )
  })
  
  output$renderInputTableoptions <- renderUI({
    verticalLayout(
      tagList(
        textInput(ns("newcolumnname"), "Ingresa nombre de la columna"),
        selectInput(ns("newcolumntype"), "Tipo de dato de columna", c("character", "integer", "double")),
        actionButton(ns("addcolumn"), "Agregar columna"),
        numericInput(ns("rowsToInput"), "Num. de renglones", value = 1, min = 1),
        actionButton(ns("addrow"), "Agregar renglones"),
        tags$div(style='padding-top:60px;', 
                 actionButton(ns("deleteTable"), "Borrar Tabla"),
                 downloadButton(ns("saveTable"), "Guardar Tabla"))
       
      )
    )
  })
  
  output$tableUI <- renderDataTable({
    DF <- getAvailableDF()
    return(DF)
  })
 
  output$inputTableUI <- renderRHandsontable({
    if(is.null(tableValues$table)) {
      tableValues$table <- data.frame(stringsAsFactors = FALSE)
    }
    return(rhandsontable(tableValues$table)) 
  })
  
  output$pivoteTable <- renderRpivotTable({
    if(input$mode == "Archivo") {
       return(rpivotTable(dataframe()))
    } else {
      if(is.null(tableValues$table) || length(tableValues$table)<1) {
        tableValues$table <- data.frame(stringsAsFactors = FALSE)
      } else {
        tableValues$table <- hot_to_r(input$inputTableUI)
      }
      return(rpivotTable(tableValues$table)) 
    }
  })

  dataframe <- reactive({
  
    filelocation <- input$file$datapath
    
    if(is.null(filelocation))  {
      return(NULL) 
    } 
    df <- NULL
    df <- read_delim(input$file$datapath,
                     col_names = input$header,
                     delim = input$sep,
                     quote = input$quote,
                     na = "NA")
    if(input$rownames) {
      df <- df[,-1]
    }
    df <- df %>% na.omit()
    return(df)
  })
  
  getAvailableDF <- function() {
    DF <- NULL
    if(input$mode == "Archivo") {
      DF <- dataframe()
    } else {
      tableValues$table <- hot_to_r(input$inputTableUI)
      DF <- tableValues$table
    }
    return(DF)
  }
 
  observeEvent(input$addcolumn, {
    DF <- tableValues$table
    
    numberOfRows <- nrow(DF)
    numberOfColumn <- ncol(DF)
    mode <- input$newcolumntype
    if(mode == "double")
      mode<- "numeric"
    
    if(numberOfColumn == 0) {
      newcolumn <- vector(mode = mode, 1)
      DF <- data.frame( newcolumn, stringsAsFactors=FALSE)
      names(DF) <- input$newcolumnname
    } else {
      DF <- hot_to_r(input$inputTableUI) 
      if( input$newcolumnname %in% colnames(DF)) {
        showNotification(sprintf("La columna '%s' ya existe.",input$newcolumnname),closeButton = TRUE, type = "error")
      } else {
        newcolumn <- vector(mode = mode, numberOfRows)
        DF <- cbind(DF, newcolumn, stringsAsFactors=FALSE)
        names(DF)[numberOfColumn + 1] <-input$newcolumnname
        updateTextInput(session, ns("newcolumnname"), label = "Ingresa nombre de la columna", value = "")
      }
    }
    
    tableValues$table <- DF
  })
  observeEvent(input$addrow, {
      tmpDataFrame <- tableValues$table
      numberOfColumn <- ncol(tmpDataFrame)
      if(numberOfColumn > 0 ) {
        DF <- hot_to_r(input$inputTableUI)
        temprow <- NULL
        if(input$rowsToInput >1) {
          temprow <- data.frame(matrix(rep(NA,input$rowsToInput * numberOfColumn),nrow=input$rowsToInput,ncol=numberOfColumn))
        } else{
          temprow <- data.frame(t(rep(NA,numberOfColumn)))
        }
        
        colnames(temprow) <- colnames(DF)
        DF <- rbind(DF, temprow)
        rownames(DF) <- NULL
        tableValues$table <- DF
      } else {
        showNotification("No hay columnas para agregar renglones",closeButton = TRUE, type = "error")
      }
  })
  
  observeEvent(input$deleteTable, {
    tableValues$table <- NULL
  })
  
  output$saveTable <- downloadHandler(
    filename = function() { filename = "data.csv" },
    content = function(file) {
      tableValues$table <- hot_to_r(input$inputTableUI)
      write.csv(tableValues$table, file, row.names = FALSE)
    }
  )
  
  output$visualizacion <- renderUI({
    tagList(
      uiOutput(ns(sprintf("imprimirSelector%s",input$tipoGrafica))),
      plotOutput(ns("mostrarGraficaVisualizacion"))
    )
  })
  
  #Histograma
  ##
  graficaHistograma <- function(){
    DF <- getAvailableDF()
    types <- sapply(DF,class)
    colName <- sprintf("%s",input$columnaHist)
    heightValues <- unlist(DF[,colName])
    par(bg = "#ccefff")
    cat(colName, "\n")
    cat(make.names(colName), "\n")
    cat(paste(types, ","), "\n")
    hist(heightValues, col="#6F45B9", ylab = "Frecuencia absoluta",
        xlab = sprintf("%s",input$columnaHist), labels = TRUE, 
        las=2, main ='')
    axis(1, labels = FALSE)
  }
  output$imprimirSelectorHistograma <- renderUI({
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    selectInput(ns("columnaHist"),"columna", columnas)
  })
  
  #Barras
  ##
  output$imprimirSelectorBarras <- renderUI( {
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    tagList(selectInput(ns("valoresX"),"etiquetas", columnas),
            selectInput(ns("valoresY"),"valores", columnas))
  })
  graficaBarras <- function(){
    DF <- getAvailableDF()
    colName <- make.names(sprintf("%s",input$valoresY))
    cat(colName, "\n")
    cat(make.names(colName), "\n")
    heightValues <- data.frame(DF)[,colName]
    types <- sapply(DF, class)
    if(types[sprintf("%s",input$valoresY)] == "numeric"| 
       types[sprintf("%s",input$valoresY)] == "integer" |
       types[sprintf("%s",input$valoresY)] == "double") {
     
    } else {
      showNotification("los valores de la grafica deben ser numéricos",closeButton = TRUE, type = "error")
      return()
    }
    maxy <- max(heightValues) + 3
    
    labelvalues <-  data.frame(DF)[,make.names(sprintf("%s",input$valoresX))]

    par(bg = "#ccefff")
    barpos <- barplot(height = heightValues, names.arg = labelvalues, col="#6F45B9", legend.text = TRUE,
                      xlab = sprintf("%s",input$valoresX), ylab = sprintf("%s",input$valoresY),
                      las = 2, ylim  = c(0,maxy))  
    text(barpos, y = heightValues, label = heightValues, pos = 3, cex = 0.8, col = "#6F45B9")
    axis(1, labels = FALSE)
  }
  
  #Pastel
  #
  output$imprimirSelectorPastel <- renderUI( {
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    tagList(selectInput(ns("valoresX"),"etiquetas", columnas),
            selectInput(ns("valoresY"),"valores", columnas))
  })
  graficaPastel <- function(){
    DF <- getAvailableDF()
    
    types <- sapply(DF, class)
    if(types[sprintf("%s",input$valoresY)] == "numeric"| 
       types[sprintf("%s",input$valoresY)] == "integer" |
       types[sprintf("%s",input$valoresY)] == "double") {
      
    } else {
      showNotification("los valores de la grafica deben ser numéricos",closeButton = TRUE, type = "error")
      return()
    }
    
    slices <- unlist(DF[,sprintf("%s",input$valoresY)])
    
    #total <- sum(slices)
   
    labelsValues <- data.frame(DF[,sprintf("%s",input$valoresX)])

    newLabels <- labelsValues %>%
      mutate(vlabel = sprintf("%s %.2f%%",labelsValues[,1], slices/sum(slices)*100)) %>% 
                 select(vlabel) %>% 
      unlist()
    
    par(bg = "#ccefff")
    pie(slices, labels = newLabels, main = "Pastel", bg =NA)
  }
  
  #Ojiva
  #
  output$imprimirSelectorOjiva <- renderUI( {
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    selectInput(ns("valoresY"),"valores", columnas)
  })
  graficaOjiva <- function(){
    DF <- getAvailableDF()
    
    types <- sapply(DF,typeof)
    colName <- sprintf("%s",input$valoresY)
    if(types[sprintf("%s",input$valoresY)] == "numeric" | 
       types[sprintf("%s",input$valoresY)] == "integer" |
       types[sprintf("%s",input$valoresY)] == "double") {
      ojivaColumn <- unlist(DF[colName])
      
      par(bg = "#ccefff")
      tabla <- table(ojivaColumn)
      plot(sort(unique(ojivaColumn)), 
           cumsum(prop.table(tabla)), 
           ylim = c(0,1), type = "lines", lwd = 2.5, col = "#3366ff",
           xlab = colName, ylab = "Frecuencia Relativa Acumulada")
      
    } else {
      showNotification("los valores de la grafica deben ser numericos",closeButton = TRUE, type = "error")
    }
  }
  
  #Boxplot
  #
  output$imprimirSelectorBoxplot <- renderUI( {
    #vacio porque no se selecciona nada solo se imprime
  })
  graficaBoxplot <- function(){
    DF <- getAvailableDF()
    
    newDF <- data.frame(lapply(DF, function(x) as.numeric(as.character(x))))
    par(bg = "#ccefff")
    boxplot(newDF, main = "Boxplot", las = 2)
  }
  
  #Tallo y hojas
  #
  output$'imprimirSelectorTallo y Hojas' <- renderUI( {
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    selectInput(ns("valoresY"),"Valores", columnas)
  })
  'graficaTallo y Hojas' <- function(){
    DF <- getAvailableDF()
    
    types <- sapply(DF,typeof)
    dataColumn <- unlist(lapply(DF[sprintf("%s",input$valoresY)], as.double) ) 
    
    textStem <- capture.output( stem(dataColumn))
    textList <- unlist(strsplit(textStem, "\n"), use.names = FALSE)
    numText <- length(textList)
    
    if((types[sprintf("%s",input$valoresY)] == "numeric"| 
       types[sprintf("%s",input$valoresY)] == "integer" |
       types[sprintf("%s",input$valoresY)] == "double") ) {
        
       
        
        
        par(bg = "#ccefff", mar = c(0,0,0,0))
        plot(c(0, numText), c(0, numText), bty = "n", ylab = "", xlab = "", type = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0,3))
        for (i in 1:numText) {
          if (i==1){
            text(x=0,y=(numText - i +1),sprintf("%s",textList[i]), pos = 4)
          }else{
            text(x=0.5,y=(numText - i +1),sprintf("%s",textList[i]), pos = 4)
          }
        }
    } else {
      showNotification("los valores de la gráfica deben ser númericos y mas de 1",closeButton = TRUE, type = "error")
    }
  }
  
  #Gráfico De puntos
  #
  output$'imprimirSelectorGráfico De puntos' <- renderUI( {
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    selectInput(ns("valoresY"),"Valores", columnas)
  })
  'graficaGráfico De puntos' <- function(){
    DF <- getAvailableDF()
    types <- sapply(DF,typeof)
    colName <- sprintf("%s",input$valoresY)
    if(types[colName] == "numeric"| 
       types[colName] == "integer" |
       types[colName] == "double") {
      valores <- unlist(DF[input$valoresY]) 
      par(bg = "#ccefff")
      stripchart(valores, method = "stack", offset = .5, at = .15, pch = 19, col= "blue", main = 'Gráfico de Puntos')
    } else {
      showNotification("los valores de la gráfica deben ser númericos",closeButton = TRUE, type = "error")
    }
  }
  
  #Dispersión
  #
  output$imprimirSelectorDispersión <- renderUI( {
    DF <- getAvailableDF()
    columnas <- colnames(DF)
    tagList(
      selectInput(ns("valoresX"),"Valores x", columnas),
      selectInput(ns("valoresY"),"Valores Y", columnas)
    )
  })
  graficaDispersión <- function(){
    DF <- getAvailableDF()
    types <- sapply(DF,typeof)
    colName <- sprintf("%s",input$valoresY)
    colNameX <- sprintf("%s",input$valoresX)
    if((types[colName] == "numeric"| 
       types[colName] == "integer" |
       types[colName] == "double")&(
       types[colNameX] == "numeric"| 
       types[colNameX] == "integer" |
       types[colNameX] == "double"
       )){
        valoresX <- unlist(DF[colNameX]) 
        valoresY <- unlist(DF[colName]) 
        medianY <- median(valoresY)
        medianX <- median(valoresX)
        par(bg = "#ccefff")
        plot(valoresX, valoresY, main = "Dispersión", xlab = colNameX, ylab = colName)
        abline(v=medianX, col="blue")
        abline(h=medianY, col="red")
    } else {
      showNotification("los valores de la gráfica deben ser númericos",closeButton = TRUE, type = "error")
    }
  }
  
  
  output$mostrarGraficaVisualizacion <- renderPlot({
    eval(call(sprintf("grafica%s",input$tipoGrafica)))
  })
  
  output$guardarExport <- downloadHandler(
    filename = sprintf("grafica%s.png", input$tipoGrafica),
    content = function(file) {
      png(file)
      eval(call(sprintf("grafica%s",input$tipoGrafica)))
      dev.off()
    }, 
    contentType = "image/png") 
  
  output$codigo <- renderUI({
    codigoMostrar <- input$tipoGrafica
    #"Histograma","Barras","Pastel","Ojiva","Boxplot", "Tallo y Hojas", "Gráfico De puntos", "Dispersión"
    if(codigoMostrar == "Histograma") {
      return(tags$pre(tags$code(
"## codigo para imprimir histograma

hist(df = Vector de valores ,
    ylab = 'Frecuencia', 
    xlab = 'Etiqueta del Eje X, labels = TRUE)")))
      }else if(codigoMostrar == "Barras") {
        return(tags$pre(tags$code("
##Codigo para imprimir Grafica de barras

barplot(height = Vector con valores numericos, 
        names.arg = Vector de etiquetas, 
        xlab = 'Etiqueta del Eje X', 
        ylab = 'Etiqueta del Eje Y')")))
      }else if(codigoMostrar == "Pastel") {
        return(tags$pre(tags$code("
##Codigo para mostrar Grafica de pastel
pie(x = Vector de valores no negativos a graficar, 
    labels = Lista de etiquetas, 
    main = 'Titulo De Pastel')")))
      }else if(codigoMostrar == "Ojiva") {
        return(tags$pre(tags$code("
##Codigo para mostrar Grafica de Ojiva

tabla <- table(x = ValoresX)

plot(x = sort(unique(Vector de valores)), ## Valores numericos unicos ordenados
    y = cumsum(prop.table(tabla)), ## Frecuencia acumulada previamente guardada en la variable tabla
    ylim = c(0,1), type = 'lines', lwd = 2.5,
    xlab = 'Etiqueta del Eje X', 
    ylab = 'Frecuencia Relativa Acumulada')
 ")))
      }else if(codigoMostrar == "Boxplot") {
        return(tags$pre(tags$code("
##Codigo para mostrar Grafica de boxplot

boxplot(x = Datos o vector numerico, 
    main = 'Titulo del Boxplot')")))
      }else if(codigoMostrar == "Tallo y Hojas") {
        return(tags$pre(tags$code("
##Codigo para mostrar los tallos y hojas

stem(x = Vector de valores numericos)")))
      }else if(codigoMostrar == "Gráfico De puntos") {
        return(tags$pre(tags$code("

##Codigo para mostrar graficos de puntos

stripchart(valores, method = 'stack', offset = .5, at = .15, pch = 19, col= 'blue', main = 'Titulo')")))
      }else if(codigoMostrar == "Dispersión") {
        return(tags$pre(tags$code("

##Codigo para mostrar la grafica de dispersion

medianY <- median(x = valores Y) ## Calcular la mediana de Y
medianX <- median(x = valores X) ## Calcular la mediana de X

plot(x = valoresX, y = valoresY, 
    main = 'Titulo de la Grafica', 
    xlab = 'Etiqueta del Eje X', ylab = 'Etiqueta del Eje Y')

abline(v = medianX, col='blue') ## Agrega linea vertical con la mediana de X
abline(h = medianY, col='red') ## Agrega linea horizontal con la mediana de Y")))
  }

      
  })
}

graph.freq <- function (x, breaks=NULL, nclass=NULL, counts = NULL) {
  if (is.numeric(x) & is.null(counts)) {
    x<-na.omit(x)
    # histogram
    if (is.null(nclass)) {
      if (is.null(breaks)) {
        breaks <- sturges.freq(x)$breaks
      }
    } else {
      breaks <- sturges.freq(x,k=nclass)$breaks
    }
    
    k<-length(breaks)
    n<- length(x)
    counts <- rep(0,k-1)
    for (i in 1:n) {
      for (j in 1:(k-2)) {
        if( (x[i] >= breaks[j]) && (x[i] < breaks[j + 1])) counts[j]<-counts[j]+1
      }
    }
    for (i in 1:n) {
      if( (x[i] >= breaks[k-1]) && (x[i] <= breaks[k])) counts[k-1]<-counts[k-1]+1
    }
    k <- length(counts)
    mids <- rep(0, k)
    ancho <- rep(0, k)
    for (i in 1:k) {
      mids[i] <- (breaks[i] + breaks[i + 1])/2
      ancho[i] <- (breaks[i + 1] - breaks[i])
    }
    altura <- round(1.1 * max(counts), 0)
  } else  {
    if( is.list(x)) {
      breaks<- x$breaks
      counts <- x$counts
    } else
      breaks <- x
    k<-length(counts)
    mids<-rep(0,k)
    ancho<-rep(0,k)
    for (i in 1:k) {
      mids[i]<-(breaks[i]+breaks[i+1])/2
      ancho[i]<-(breaks[i+1]-breaks[i])
    }
  }

  a<-breaks[1]-ancho[1]/2
  b<-breaks[k+1]+ancho[k]/2
  relative<-round(counts/sum(counts),4)
  density <- relative/ancho
  histogram<-structure(list(breaks=breaks,counts=counts,mids=mids,relative=relative,density=density),class="graph.freq")
  histogram
}

sturges.freq <-function (x,k=0) {
    n <- length(x)
    if (k==0) k <- round(1+log(n,2),0)
    p<- floor(log(abs(median(x,na.rm=TRUE)),10))
    x<-x/10^(p-1)
    maximo <- max(x,na.rm=TRUE)
    minimo <- min(x,na.rm=TRUE)
    min1<-floor(minimo)
    max1<-ceiling(maximo)
    amplitud <- max1 - min1
    tic <- round(amplitud/k,1)
    clases <- seq(min1, max1, tic)
    if (maximo > clases[length(clases)]) {
      clases <- c(clases, clases[length(clases)] + tic)
    }
    k <- length(clases)-1
    maximo<-maximo*10^(p-1);minimo<-minimo*10^(p-1);tic=tic*10^(p-1)
    clases<-clases*10^(p-1); amplitude=amplitud*10^(p-1)
    lista <- list(maximum = maximo, minimum = minimo, amplitude = amplitud, 
                  classes = k, interval = tic, breaks = clases)
    return(lista)
}

table.freq <- function(object) {
    xx<-object$mids
    yy<-object$counts
    y1<-sum(yy)
    zz<-object$breaks
    x<-length(xx)
    acum<-0
    z<-rep(0,7*x)
    dim(z)<-c(x,7)
    for (i in 1:x) {
      z[i,1]<-zz[i]
      z[i,2]<-zz[i+1]
      z[i,3]<-xx[i]
      z[i,4]<-yy[i]
      z[i,5]<-round(yy[i]*100/y1,1)
      z[i,6]<-yy[i]+acum
      acum<-z[i,6]
      z[i,7]<-round(z[i,6]*100/y1,1)
    }
    z[nrow(z),7]<-100
    colnames(z)<-c("Lower","Upper","Main","Frequency","Percentage","CF","CPF")
    z<-as.data.frame(z)
    invisible(z)
}

