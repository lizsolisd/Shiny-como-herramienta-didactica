# Prueba de hipotesis
mostrarPantallaHipotesis <- function(id= "hipotesis", label = "hipotesis") {
  ns <- NS(id)
  uiOutput(ns("FrameGeneral"))
}

mostrarHipotesisOptions <- function(id = "hipotesis", label = "hipotesis") {
  ns <- NS(id)
  verticalLayout(
    uiOutput(ns("MEDIA1")),
    #uiOutput(ns("SLIDER1")),
    uiOutput(ns("MEDIA2")),
    hr( color="gray" ),
    uiOutput(ns("input_n")),
    numericInput(ns("varianza"), "σ² = ", value = 1, min = 0.1, step = 0.01),# debe ser  >0
    numericInput(ns("alpha_error"), "α = ", value = .05, min = 0, max = 1, step = .005),
    checkboxInput(ns("AutoAdjust"), "Auto Ajustar Y en Grafica", value = TRUE),
    checkboxInput(ns("showh0"), "Mostrar H0", value = TRUE),
    checkboxInput(ns("showh1"), "Mostrar H1", value = TRUE)
    #uiOutput(ns("SLIDER2"))
  )
}

setupHipotesisListeners <- function(input, output, session, label = "hipotesis") {
  ns <- session$ns
  color_azul <- "#3366ff"
  color_rojo <- "#ff007f"
  ##866bff
  ##0080ff
  value <- reactiveValues(k = 8, error_t2 =0)
  
  
  output$MEDIA1 = renderUI({
    numericInput(ns("mu_0"), label = tags$div(style = "color:#3366ff","H0: µ ="), value = 1, step = 0.5)
  })
  output$MEDIA2 = renderUI({
    numericInput(ns("mu_1"),label = tags$div(style = "color:#ff007f", "H1: µ ="), value = 4, step = 0.5)
  })
  
  output$input_n = renderUI({
    numericInput(ns("input_n"), "n =", value = 1, min = 1, step = 1)
  })

  output$FrameGeneral <- renderUI ({
    tagList(h2("Prueba de hipótesis simple para la media de una distribución Normal con varianza conocida"),
    
      plotOutput(ns("mostrarGrafica")),
      div(style="display: inline-block;vertical-align:top;",uiOutput(ns("mostrarInformacion"))),
      div(style="display: inline-block;min-width: 600px;",plotOutput(ns("graficaPotencia")))
      )
  })
  
  output$mostrarGrafica <- renderPlot({
    
    ##Inputs
    varianza <- input$varianza
    if(!is.numeric(varianza) || !varianza > 0) {
      showNotification("Desviación debe ser un número válido > 0",closeButton = TRUE, type = "error")
      varianza <- 1
    }
    
    ## Validar n sea entero positivo
    
    n <- input$input_n
    if(!is.integer(n) || !n > 0) {
      showNotification("n debe ser entero positivo",closeButton = TRUE, type = "error")
      n <- 1
    }
    
    desviacion <- sqrt(varianza/n)
   
    amplitud <- 5 * desviacion

    #Leer medias bajo ambas hipotesis
    if(input$mu_1 > input$mu_0){
      mu_der <- input$mu_1
      mu_izq <- input$mu_0
      NombreDerecha <- "H1"
      NombreIzquierda <- "H0"
      colorIzq <- color_azul
      colorDer <- color_rojo
      text_izq <- "µ0"
      text_der <- "µ1"
    }else{
      colorIzq <- color_rojo
      colorDer <- color_azul
      mu_izq <- input$mu_1
      mu_der <- input$mu_0
      NombreIzquierda <- "H1"
      NombreDerecha <- "H0"
      text_izq <- "µ1"
      text_der <- "µ0"
    }


    ##Generar valores de Xini y Y para graficar posteriormente
    xValues_izq <- seq(mu_izq - amplitud, mu_izq + amplitud, length = 2000)
    yValues_izq <- dnorm(xValues_izq, mu_izq, desviacion)
    xValues_der <- seq(mu_der - amplitud, mu_der + amplitud, length = 2000)
    yValues_der <- dnorm(xValues_der, mu_der, desviacion)
    
    ##Leer nivel de confianza: ALPHA
    alpha_error <- input$alpha_error
    if(!is.numeric(alpha_error) || !alpha_error > 0 || alpha_error >1) {
      showNotification("Alpha debe ser un número válido 0 < alpha > 1",closeButton = TRUE, type = "error")
      alpha_error <- 0.05
    }
    
    ##Valores criticos: 
    z <- qnorm(1-alpha_error, mean = 0, sd = 1)
    if(input$mu_1 > input$mu_0){
      k_izq <- input$mu_0+z*desviacion
      }
    else{
      k_izq <- input$mu_0-z*desviacion
    }
    value$k <- k_izq
    #k_izq <- mu_izq + (qnorm(1 - alpha_error) *desviacion)
    #value$k <- qnorm(alpha_error, mean = mu_izq, sd = desviacion) 
    
    #Rellena áreas (alpha)
    fillAreaX_izq <- xValues_izq[xValues_izq >= k_izq]
    fillAreaY_izq <- dnorm(fillAreaX_izq, mu_izq,desviacion)
    fillAreaY_izq[1] <- 0
    

    #Rellena área (Beta)
    fillAreaX_der <-  xValues_der[xValues_der <= k_izq]
    fillAreaY_der <-  dnorm(fillAreaX_der, mu_der,desviacion)
    fillAreaY_der[length(fillAreaY_der)] <- 0
    
    #ajuste visual de grafica
    ylim <- c(0, max(yValues_izq) *1.2)
    if(!input$AutoAdjust) {
      ylim <- c(0, 1.3)
    }
    xmin <- xValues_izq[1]
    xmax <- xValues_der[2000]
    
    #plot both
    plot(xValues_izq, yValues_izq, type="line", xlab = "", ylab = "", lwd=0 ,
         xlim = c(xmin, xmax), ylim =ylim)
    if(input$showh0) {
      lines(xValues_izq, yValues_izq, type="l", lwd=2.5)
    }
    if(input$showh1) {
      lines(xValues_der, yValues_der, type = "l", lwd=2.5)
    }
    abline(h = 0, col = "black", lwd = 1)
    
    # mostrar area rellena
    if(input$showh0) {
      polygon(fillAreaX_izq, fillAreaY_izq, col = colorIzq, density = 40)
      legend("topleft", legend=c("α"), fill= color_azul, density = 40,  bty="n",cex = 1.5) 
    }
    if(input$showh1) {
      polygon(fillAreaX_der, fillAreaY_der, col = colorDer, density = 20)
      legend("topright", legend=c("ß"), fill= color_rojo, density = 30,  bty="n",cex = 1.5) 
    }
    
    # mostrar lineas de medias
    if(input$showh0) {
      lines(c(mu_izq,mu_izq), c(0,max(yValues_izq)), lwd=2, col=colorIzq)
    }
    if(input$showh1) {
      lines(c(mu_der,mu_der), c(0,max(yValues_der)), lwd=2, col=colorDer)
    }
    
    # mostrar titulo sobre grafica de media
    if(input$showh0) {
      text(mu_izq, max(yValues_izq), NombreIzquierda, pos = 3, col = colorIzq, font = 2)
    }
    if(input$showh1) {
      text(mu_der, max(yValues_der), NombreDerecha, pos = 3, col = colorDer, font = 2)
    }
    
    #mostrar media y alpha

    if(input$showh1) {
      mtext(text_der, side = 1, at = mu_der, padj = 5, font = 2, cex = 1.2, col = colorDer)
    }

    mtext(sprintf("%.2f",value$k), side = 1, at = value$k, font = 2, cex = 1.2, padj = 3, col = 'darkcyan')
    
    if(input$showh0) {
      mtext(text_izq, side = 1, at = mu_izq, padj = 5, font = 2, cex = 1.2, col = colorIzq)
    }
    
    if(input$mu_1 > input$mu_0){
      value$error_t2 <- pnorm(q = value$k, mean = input$mu_1, sd = desviacion)
    }else{
      value$error_t2 <- 1-pnorm(q = value$k, mean = input$mu_1, sd = desviacion)
    }
    
  })
  
  output$mostrarInformacion <- renderUI({
    confianza <- input$confianza
    tagList(tags$div(style="padding: 10px;",
        tags$div(style="top: 0;",
          h3("Error Tipo I"),
          h4(sprintf("alpha = %s",input$alpha_error))),
        tags$div(style="top: 0;",
          h3("\nError Tipo II"),
          h4(sprintf("beta = %s",round(value$error_t2,4)))),
        tags$div(style="top: 0;",
           h3("\nFunción potencia en μ1"),
           h4(sprintf("1-beta= %s",1-round(value$error_t2,4))))
    ))
  }) 
  
  output$graficaPotencia <- renderPlot({
    varianza <- input$varianza
    n <- input$input_n
    desviacion <- sqrt(varianza/n)
    alpha_error <- input$alpha_error
    ylim <- c(0, 1.1)
    

    yValues <- c()
    if(input$mu_0 < input$mu_1){
      xValues<-c(input$mu_0, input$mu_1)
      for(x in xValues) {
        newVal <- pnorm(q = value$k, mean = x, sd = desviacion)
        yValues <- c(yValues, 1-newVal)
      }
    }else{
      xValues<-c(input$mu_1, input$mu_0)
      for(x in xValues) {
        newVal <- pnorm(q = value$k, mean = x, sd = desviacion)
        yValues <- c(yValues, newVal)
      }
    }
    
    cat(xValues[1], " - ", tail(xValues, n=1), "\n")
    xLim <- c(xValues[1]-1, tail(xValues, n=1)+1)
    plot(xValues, yValues, ylim=ylim, xlim = xLim, ylab = "Potencia", 
         xlab = "", main = "Función Potencia",pch = 19, lwd=2)
    
    text(input$mu_0, alpha_error + 0.05, "α", srt=0.2, col = "purple", cex =1.5)
    text(input$mu_1, setdiff(round(yValues,2), alpha_error) + 0.05, "1-ß", srt=0.2, col = 'darkcyan', cex = 1.5)
    
    mtext("µ0", side = 1, at = input$mu_0, padj = 5, font = 2, cex = 1.2, col = 'purple')
    mtext("µ1", side = 1, at = input$mu_1, padj = 5, font = 2, cex = 1.2, col = 'darkcyan')
    
    
  })
  
}
