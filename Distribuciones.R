#Distribuciones
library(dplyr)
renderDistribuciones <- function(id = "distribuciones", label = "distribuciones") {
  ns <- NS(id)
  uiOutput(ns("FrameGeneral"))
}

renderDistribucionesOptions <- function(id = "distribuciones", label = "distribuciones") {
  ns <- NS(id)
  return(tags$div(style = "padding-left: 10px;",
    verticalLayout(
      #discretas
      h6("Discretas"),
      actionLink(ns("probBernoulli"), "Bernoulli"),
      actionLink(ns("probBinomial"), "Binomial"),
      actionLink(ns("probPoisson"), "Poisson"),
      #continuas
      h6("Continuas"),
      actionLink(ns("probUniforme"), "Uniforme"),
      actionLink(ns("probNormal"), "Normal"),
      actionLink(ns("probExponencial"), "Exponencial"),
      actionLink(ns("probGamma"), "Gamma"),
      actionLink(ns("probji2"), "Ji-Cuadrada"),
      actionLink(ns("probT"), "T"),
      actionLink(ns("probF"), "F")
    )))
}

setupDistribucionesListeners <- function(input, output, session, label = "distribuciones") {
  ns <- session$ns
  tema <- reactiveValues(seleccion = NULL, discreta = TRUE)
  
  output$tabPanel <- renderUI({
    cat(sprintf("render %s\n",tema$seleccion))
    tituloEstadistico <- "Estadísticos"
    if(tema$seleccion == "F" || tema$seleccion == "T") {
      tituloEstadistico <- "Valores Críticos"
    }
    tmptitle <- "Masa de Probabilidad"
    if(!tema$discreta) {
      tmptitle <- "Densidad"
    }
    tabEstadistico <- tabPanel(tituloEstadistico,
                               tableOutput(ns(sprintf("Summary%s",tema$seleccion))),
                               tableOutput(ns(sprintf("Estadisticos%s",tema$seleccion))))
    
    tabTabulacion <- tabPanel("Tabulación",
                              tableOutput(ns(sprintf("Tabulacion%s",tema$seleccion))))
    tabProb <- tabPanel(paste0("Función de ",tmptitle),
                           uiOutput(ns("graficaProbabilidades")))
    tabAcumu <- tabPanel("Función de Distribución Acumulada",
                            uiOutput(ns("graficaAcumulada")))
    
    if(isTRUE(tema$discreta)) {
      return(tabsetPanel(tabEstadistico, tabTabulacion,tabProb, tabAcumu ))
    }else{
      return(tabsetPanel(tabEstadistico,tabProb, tabAcumu ))
    }
  })
  
  
  ## Bernoulli
  ## 
  observeEvent(input$probBernoulli, {
    
    tema$seleccion <- "Bernoulli"
    tema$discreta <- TRUE
    
    output$TabulacionBernoulli <- renderTable({
      ## Indique la Probabilidad de éxito:
      proba_exito <- input$probExito
      
      ## Número de intentos o ensayos del experimento:
      n_intentos <- 1
      
      ## Contenido
      x1 <- c(0:n_intentos)
      
      ## Tabla con probabilidades
      return (data.frame(x = x1, Probabilidad = round(dbinom(x1, size= n_intentos, prob = proba_exito, log = FALSE), 7),
                                 Probabilidad_Acumulada = round(pbinom(x1, size= n_intentos, prob = proba_exito),7)) )
    }, digits = 5)
  
    
    output$SummaryBernoulli <- renderTable({
      
      ## Indique la Probabilidad de éxito:
      proba_exito <- input$probExito
     
      ## Número de intentos o ensayos del experimento:
      n_intentos <- 1
      
      ## Contenido
      x1 <- c(0:n_intentos)
      
      ## Estadisticos
      return (data_frame(minimo = min(x1),
                            primer_cuartil =   qbinom(.25,size= n_intentos, prob = proba_exito),
                            mediana = qbinom(.5,size= n_intentos, prob = proba_exito),
                            tercer_cuartil =   qbinom(.75,size= n_intentos, prob = proba_exito),
                            maximo = max(x1)))
      
    }, digits = 5)
    
    output$EstadisticosBernoulli <- renderTable({
      
      ## Indique la Probabilidad de éxito:
      proba_exito <- input$probExito
     
      ## Número de intentos o ensayos del experimento:
      n_intentos <- 1
      
      ## Contenido
      x1 <- c(0:n_intentos)
      
      media <- proba_exito*n_intentos
      varianza <- proba_exito*n_intentos*(1-proba_exito)
      
      return  (data_frame(media = media, varianza = varianza))
    })
    
  })
  
  graficaBernoulli <- function(){
    ## Indique la Probabilidad de éxito:
    proba_exito <- input$probExito
    if(!isNumeric(proba_exito)) {
      showNotification("La pobabilidad debe ser un valor numerico entre 0 y 1",closeButton = TRUE, type = "error")
      proba_exito <- 0
    } else {
      if(proba_exito > 1) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 1
      } else if(proba_exito <0) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 0
      }
    }
    
    
    ## Número de intentos o ensayos del experimento:
    n_intentos <- 1
    
    ## Contenido
    x1 <- c(0:n_intentos)
    
    ## Tabla con probabilidades
    tabulacion <- data.frame(x = x1, 
                             Probabilidad = round(dbinom(x1, size= n_intentos, prob = proba_exito, log = FALSE), 7),
                             Probabilidad_Acumulada = round(pbinom(x1, size= n_intentos, prob = proba_exito),7) ) 
    
    ## Distribucion de probabilidad
    plot(x = tabulacion$x, y =tabulacion$Probabilidad, xlab="x", ylab="Probabilidad", 
         col="blue", lwd=1.5, main="Función de Masa de Probabilidad", type = "h", lty = 3) 
    points(tabulacion$x, tabulacion$Probabilidad, pch = 19, col = "blue") 
  }
  
  graficaAcumuladaBernoulli <- function() {
    ## Indique la Probabilidad de éxito:
    proba_exito <- input$probExito
    if(!isNumeric(proba_exito)) {
      showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
      proba_exito <- 0
    } else {
      if(proba_exito > 1) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 1
      } else if(proba_exito <0) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 0
      }
    }
    ## Número de intentos o ensayos del experimento:
    n_intentos <- 1
    
    ## Contenido
    x1 <- c(1:n_intentos)
    
    ## Tabla con probabilidades
    valoresY <-  pbinom(c(0,1:n_intentos), size=n_intentos, prob=proba_exito)
    
    plot(stepfun(x1, valoresY, right = TRUE) , 
         verticals = FALSE, xlab = "x", ylab = "Probabilidad", col="purple", lwd=1.5, 
         main="Función de Distribución Acumulada", pch = 1)
  }
  
  output$optionsBernoulli <- renderUI({
     return(numericInput(ns("probExito"), "Probabilidad de éxito", value = 0.5, min = 0, max = 1, step = 0.1))
  })
  
  ## Binomial
  ## 
  observeEvent(input$probBinomial, {
    tema$seleccion <- "Binomial"
    tema$discreta <- TRUE
    
    output$TabulacionBinomial <- renderTable({
      ## Indique la Probabilidad de éxito:
      proba_exito <- input$probExito
      ## Número de intentos o ensayos del experimento:
      n_intentos <- input$numeroIntentos
      
      ## Contenido
      x1 <- c(0:n_intentos)
      
      ## Tabla con probabilidades
      return (data.frame(x = x1, Probabilidad = round(dbinom(x1, size= n_intentos, prob = proba_exito, log = FALSE), 7),
                                 Probabilidad_Acumulada = round(pbinom(x1, size= n_intentos, prob = proba_exito),7) ) )
    } , digits = 5)
    
    output$SummaryBinomial <- renderTable({
      
      ## Indique la Probabilidad de éxito:
      proba_exito <- input$probExito
      
      ## Número de intentos o ensayos del experimento:
      n_intentos <- input$numeroIntentos
      
      ## Contenido
      x1 <- c(0:n_intentos)
      
      ## Estadisticos
      return  (data_frame(minimo = min(x1),
                            primer_cuartil =   qbinom(.25,size= n_intentos, prob = proba_exito),
                            mediana = qbinom(.5,size= n_intentos, prob = proba_exito),
                            tercer_cuartil =   qbinom(.75,size= n_intentos, prob = proba_exito),
                            maximo = max(x1)))
    }, digits = 5)
    
    output$EstadisticosBinomial <- renderTable({
      
      ## Indique la Probabilidad de éxito:
      proba_exito <- input$probExito
      
      ## Número de intentos o ensayos del experimento:
      n_intentos <- input$numeroIntentos
      
      ## Contenido
      media <- proba_exito*n_intentos
      varianza <- proba_exito*n_intentos*(1-proba_exito)
      
      return (data_frame(media = media, varianza = varianza))
      
    })
    
  })
  
  graficaBinomial <- function() {
    ## Indique la Probabilidad de éxito:
    proba_exito <- input$probExito
    if(!isNumeric(proba_exito)) {
      showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
      proba_exito <- 0
    } else {
      if(proba_exito > 1) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 1
      } else if(proba_exito <0) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 0
      }
    }
    
    ## Número de intentos o ensayos del experimento:
    n_intentos <- input$numeroIntentos
    if(!isNumeric(n_intentos)) {
      showNotification("El número de intentos debe ser un valor numérico",closeButton = TRUE, type = "error")
      n_intentos <- 0
    } else if(n_intentos < 0) {
      showNotification("El número de intentos debe ser mayor que 0",closeButton = TRUE, type = "error")
      n_intentos <- 0
    }
    ## Contenido
    x1 <- c(0:n_intentos)
    
    ## Tabla con probabilidades
    tabulacion <- data.frame(x = x1, 
                             Probabilidad = round(dbinom(x1, size= n_intentos, prob = proba_exito, log = FALSE), 7),
                             Probabilidad_Acumulada = round(pbinom(x1, size= n_intentos, prob = proba_exito),7) ) 
    
    ## Distribucion de probabilidad
    plot(x = tabulacion$x, y =tabulacion$Probabilidad, xlab="x", ylab="Probabilidad", 
         col="blue", lwd=1.5, main="Función de Masa de Probabilidad", type = "h",  pch = 19, lty = 3, cex = 5)
    points(tabulacion$x, tabulacion$Probabilidad, pch = 19, col = "blue") 
  }
  
  graficaAcumuladaBinomial <- function() {
    ## Indique la Probabilidad de éxito:
    proba_exito <- input$probExito
    if(!isNumeric(proba_exito)) {
      showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
      proba_exito <- 0
    } else {
      if(proba_exito > 1) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 1
      } else if(proba_exito <0) {
        showNotification("La probabilidad debe ser un valor numérico entre 0 y 1",closeButton = TRUE, type = "error")
        proba_exito <- 0
      }
    }
    
    ## Número de intentos o ensayos del experimento:
    n_intentos <- input$numeroIntentos
    if(!isNumeric(n_intentos)) {
      showNotification("El número de intentos debe ser un valor numérico",closeButton = TRUE, type = "error")
      n_intentos <- 0
    } else if(n_intentos < 0) {
      showNotification("El número de intentos debe ser mayor que 0",closeButton = TRUE, type = "error")
      n_intentos <- 0
    }
    
    ## Contenido
    x1 <- c(1:n_intentos)
    
    ## Tabla con probabilidades
    valoresY <-  pbinom(c(0,1:n_intentos), size=n_intentos, prob=proba_exito)
    
    plot(stepfun(x1,valoresY, right = TRUE), xlab = "x", ylab = "Probabilidad", 
         verticals = FALSE, lwd=1.5, main="Función de Distribución Acumulada", pch = 1,
         col="purple")    
  }
  
  output$optionsBinomial <- renderUI({
    return( tagList(numericInput(ns("numeroIntentos"), "Número de intentos", value = 10, step = 10, min = 0),
            numericInput(ns("probExito"), "Probabilidad de éxito", value = 0.5, min = 0, max = 1, step = 0.1)))
  })

  ## Poisson
  ## 
  observeEvent(input$probPoisson, {
    tema$seleccion <- "Poisson"
    tema$discreta <- TRUE
    output$TabulacionPoisson <- renderTable({
      l <- input$lambda
      
      ## Contenido
      x1 <- seq(from = qpois(0.000001,l,log.p = FALSE),
                to = qpois(0.999999,l,log.p = FALSE),
                length.out = 100)
      
      ## Tabla con probabilidades
      valoresY <- unique(ppois(x1,  lambda = l, log.p = FALSE))
      x <- qpois(valoresY, l)
      
      return(data.frame(x, Probabilidad = dpois(x, lambda = l, log = FALSE), 
                                Probabilidad_Acumulada = ppois(x,  lambda = l) ) )
    }, digits = 5)
    
    output$SummaryPoisson <- renderTable({
      l <- input$lambda
      ## Estadisticos
      return(data_frame(minimo = qpois(0, lambda = l),
                            primer_cuartil =   qpois(.25, lambda = l),
                            mediana = qpois(.5, lambda = l),
                            tercer_cuartil = qpois(.75, lambda = l),
                            maximo = qpois(1, lambda = l)))
    }, digits = 5)
    
    output$EstadisticosPoisson <- renderTable({
      l <- input$lambda
      ## Media de la muestra:
      media <- l
      
      ## Varianza:
      varianza <- l
      
      return(data_frame(media = media, varianza = varianza))
      
    })
    
  })
  
  graficaPoisson <- function(){
    
    l <- input$lambda
    if(!isNumeric(l)) {
      showNotification("Lambda debe ser un valor numérico",closeButton = TRUE, type = "error")
      l <- 0.000001
    } else if(l <= 0) {
      showNotification("Lambda debe ser mayor que 0",closeButton = TRUE, type = "error")
      l <- 0.000001
    }
    
    xInit = round(l -3*l, 0)
    if(xInit < 0) {
      xInit <- 0
    }
    xEnd = round(l +3*l, 0)
    ## Contenido
    x1 <- c(xInit:xEnd)
    
    ## Tabla con probabilidades
    tabulacion <- data.frame(x = x1, 
                             Probabilidad = round(dpois(x1, lambda = l, log = FALSE),7), 
                             Probabilidad_Acumulada = round(ppois(x1,  lambda = l),7) ) 
    
    
    ## Distribucion de probabilidad
    plot(x = tabulacion$x, y =tabulacion$Probabilidad, xlab="x", ylab="Probabilidad", 
         col="blue", lwd=1.5, main="Función de Masa de Probabilidad", pch = 16, type = "h", lty = 3)
    points(tabulacion$x, tabulacion$Probabilidad, pch = 19, col = "blue") 
    
  }
  
  graficaAcumuladaPoisson <- function() {
    l <- input$lambda
    if(!isNumeric(l)) {
      showNotification("Lambda debe ser un valor numérico",closeButton = TRUE, type = "error")
      l <- 0.000001
    } else if(l <= 0) {
      showNotification("Lambda debe ser mayor que 0",closeButton = TRUE, type = "error")
      l <- 0.000001
    }
    
    xInit <- qpois(0.01,l,log.p = FALSE)
    xEnd <- qpois(0.99,l,log.p = FALSE)

    ## Contenido
    x1 <- seq(from = qpois(0.000001,l,log.p = FALSE),
              to = qpois(0.999999,l,log.p = FALSE),
              length.out = 100)
    
    ## Tabla con probabilidades
    valoresY <- unique(ppois(x1,  lambda = l, log.p = FALSE))
    x <- qpois(valoresY, l)
    valoresY <- ppois(x,  lambda = l, log.p = FALSE) 
    
    if(length(valoresY) == 1) {
      x <- 0 
      valoresY <- c(0, valoresY)
    } else {
      x <- x[-length(x)]
    }
    
    
    ## Función de densidad acumulada:
    plot(stepfun(x,valoresY, right = TRUE), 
         xlab = "x", ylab = "Probabilidad", col="purple", lwd=1.5, verticals = FALSE,
         main="Función de Distribución Acumulada", pch = 1)
  }
  
  output$optionsPoisson <- renderUI({
    return(numericInput(ns("lambda"), "lambda", value = 2, min = 0, step = .1))# LAMBDA DEBE SER POSITIVA
  })
  
  
  ## Uniforme Continua
  ## 
  observeEvent(input$probUniforme, {
    tema$seleccion <- "Uniforme"
    tema$discreta <- FALSE
    
    output$SummaryUniforme <- renderTable({
      a <- min(input$limInferior, input$limSuperior)
      b <- max(input$limInferior, input$limSuperior)
      
      ## Media de la muestra:
      media <- (a+b)/2
      
      ## Varianza:
      varianza <- ((b-a)^2)/12
      
      ## Estadisticos
      return (data_frame(minimo = a,
                            primer_cuartil = qunif(.25,min = a, max = b),
                            mediana = qunif(.5,min = a, max = b),
                            tercer_cuartil = qunif(.75,min = a, max = b),
                            maximo = b))
    })
    
    output$EstadisticosUniforme <- renderTable({
      a <- min(input$limInferior, input$limSuperior)
      b <- max(input$limInferior, input$limSuperior)
      
      ## Media de la muestra:
      media <- (a+b)/2
      
      ## Varianza:
      varianza <- ((b-a)^2)/12
      
      return (data_frame(media = media, varianza = varianza))
      
    })
  })
  
  graficaUniforme <- function() {
    a <- input$limInferior
    b <- input$limSuperior 
    
    if(!(isNumeric(a) & isNumeric(b) & a < b)) {
      showNotification("Ambos valores deben ser válidos y A > B",closeButton = TRUE, type = "error")
      a <- 0
      b <- 1
    }
    
    x1 <- seq(from = a, to = b, by = (b-a)/1000)
    ## Tabla con probabilidades
    tabulacion <- data.frame(x = x1, 
                             Probabilidad = rep(1/ (b-a), length(x1) ),
                             Probabilidad_Acumulada = round((x1-a)/(b-a),7) )  
    
    
    xInit <- min(x1)
    ## Distribucion de probabilidad
    
    mostrarGraficaConValores(xInit, x1, tabulacion$Probabilidad, tabulacion$Probabilidad_Acumulada)
  }
  
  graficaAcumuladaUniforme <- function() {
    a <- input$limInferior
    b <- input$limSuperior 
    
    if(!(isNumeric(a) & isNumeric(b) & a < b)) {
      showNotification("Ambos valores deben ser válidos y el limite inferior debe ser mayor que el limite superior",closeButton = TRUE, type = "error")
      a <- 0
      b <- 1
    }
    
    x1 <- seq(from = a, to = b, by = (b-a)/50)
    ## Tabla con probabilidades
    tabulacion <- data.frame(x = x1, 
                             Probabilidad = rep(1/ (b-a), length(x1) ),
                             Probabilidad_Acumulada = round((x1-a)/(b-a),7) )  
    
    
    ## Función de densidad acumulada:
    plot(x = tabulacion$x, y = tabulacion$Probabilidad_Acumulada, xlab = "x", ylab = "Probabilidad", col="purple", lwd=2, 
         main="Función de Distribución Acumulada", type = "l??")
  }
  
  output$optionsUniforme <- renderUI({
    return( tagList(numericInput(ns("limInferior"), "a", value = 2,  step = 1),
                    numericInput(ns("limSuperior"), "b", value = 10, step = 1)))
  })
  
  ## Normal
  ## 
  observeEvent(input$probNormal, {
    tema$seleccion <- "Normal"
    tema$discreta <- FALSE
    
    output$SummaryNormal <- renderTable({
      media <- input$media
      desviacion <- input$desviacion
      ## Estadisticos
      return (data_frame(minimo = qnorm(0, mean = media, sd = desviacion),
                            primer_cuartil = qnorm(.25, mean = media, sd = desviacion),
                            mediana = qnorm(.5, mean = media, sd = desviacion),
                            tercer_cuartil = qnorm(.75, mean = media, sd = desviacion),
                            maximo = qnorm(1, mean = media, sd = desviacion)))
    })
    
    output$EstadisticosNormal <- renderTable({
      ## Inicializar valores:
      media <- input$media
      desviacion <- input$desviacion
       return(data_frame(media = media, varianza = desviacion^2))
    })
    
  })
  
  graficaNormal <- function() {
    ## Inicializar valores:
    media <- input$media
    desviacion <- input$desviacion
    if(!is.numeric(media)) {
      showNotification("µ deber ser un número real",closeButton = TRUE, type = "error")
      media <- 0
      return()
    }
    if(!is.numeric(desviacion) || !desviacion > 0) {
      showNotification("σ² debe ser mayor que 0",closeButton = TRUE, type = "error")
      desviacion <- 0.5
      return()
    }
    
    amplitud <- 10 * desviacion
    
    xInit = media - amplitud
    xEnd = media + amplitud
    
    xValues <- seq(xInit, xEnd, length = 10000)
    yValues <- dnorm(xValues, media, desviacion)
    
    yValorAcumulado <- pnorm(xValues, media, desviacion)
    
    mostrarGraficaConValores(xInit, xValues, yValues, yValorAcumulado)
  }
  
  graficaAcumuladaNormal <- function() {
    media <- input$media
    desviacion <- input$desviacion
    if(!is.numeric(media)) {
      showNotification("La media debe ser un numero válido",closeButton = TRUE, type = "error")
      media <- 0
      return()
    }
    
    if(!is.numeric(desviacion) || !desviacion > 0) {
      showNotification("Desviación debe ser un número válido > 0",closeButton = TRUE, type = "error")
      desviacion <- 0.5
      return()
    }
    
    ## Contenido
    x <- seq(from  = media-7*desviacion, to = media+7*desviacion, length.out = 1000)
    
    ## Función de densidad acumulada:
    curve(pnorm(x, mean = media, sd = desviacion), xlab="", ylab="", from = x[1], to = x[1000], col="purple", lwd=2, main="Función de Distribución Acumulada")
  }
  
  output$optionsNormal <- renderUI({
    return( tagList(numericInput(ns("media"), "µ", value = 200, min = -10000, max = 10000, step = 1),
                    numericInput(ns("desviacion"), "σ", value = 40, min = 0, max = 1000, step = 1)))
  })
  
  ## Exponencial
  ## 
  observeEvent(input$probExponencial, {
    tema$seleccion <- "Exponencial"
    tema$discreta <- FALSE
    
    output$SummaryExponencial <- renderTable({
      theta <- input$theta
      
      ## Estadisticos
      return(data_frame(minimo = qexp(0, rate = 1/theta),
                            primer_cuartil = qexp(.25, rate = 1/theta),
                            mediana = qexp(.5, rate = 1/theta),
                            tercer_cuartil = qexp(.75, rate = 1/theta),
                            maximo = qexp(1, rate = 1/theta)))
      
    })
    
    output$EstadisticosExponencial <- renderTable({
      theta <- input$theta
      ## Media de la muestra:
      media <- theta
      
      ## Varianza:
      varianza <- theta^2
      
      return (data_frame(media = media, varianza = varianza))
    })
    
  })
  
  graficaExponencial <- function() {
    #inputs
    theta <- input$theta
    if(!isNumericGreatThan0(theta)) {
      showNotification("Theta debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      theta <- 1
    }
    media <- theta
    desviacion <- theta
    muestra <- 10000
    amplitud <- 7 * desviacion
    
    xInit = media - amplitud
    if(xInit <0) {
      xInit <- -0.5
    }
    xEnd = media + amplitud
    
    #contentido
    xValues <- seq(xInit, xEnd, length = muestra)
    yValues <- dexp(xValues, rate = 1/theta)
    yValorAcumulado <- pexp(xValues, rate = 1/theta)
    
    mostrarGraficaConValores(xInit, xValues, yValues, yValorAcumulado)
  }
  
  graficaAcumuladaExponencial <- function(){
    theta <- input$theta
    if(!isNumericGreatThan0(theta)) {
      showNotification("theta debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      theta <- 1
    }
    media <- theta
    
    ## Contenido
    x <- seq(from  = round(qexp(.01, rate = 1/theta),0)-.5, 
             to = round(qexp(.99, rate = 1/theta),0)+3, 
             length.out = 1000) 
    
    
    ## Función de densidad acumulada:
    curve(pexp(x, rate=1/theta), xlab="", ylab="", from = x[1], to = x[1000], col="purple", lwd=2, main="Función de Distribución Acumulada")
  }
  
  output$optionsExponencial <- renderUI({
    return( numericInput(ns("theta"), "theta", value = 1, min = 0, step = 1))
  })
  
  #Gamma
  observeEvent(input$probGamma, {
    tema$seleccion <- "Gamma"
    tema$discreta <- FALSE
    output$SummaryGamma <- renderTable({
      alpha <- input$alpha
      theta <- input$theta
      
      ## Estadisticos
     return (data_frame(minimo = qgamma(0, shape = alpha, scale = theta ),
                            primer_cuartil = qgamma(.25,shape = alpha, scale = theta),
                            mediana = qgamma(.5, shape = alpha, scale = theta),
                            tercer_cuartil = qgamma(.75, shape = alpha, scale = theta),
                            maximo = qgamma(1, shape = alpha, scale = theta)))
    })
    
    output$EstadisticosGamma <- renderTable({
      alpha <- input$alpha
      theta <- input$theta
      ## Media de la muestra:
      media <- alpha*theta#cambiar operacion segun el parametro
      
      varianza <-  (alpha*theta^2)
      
      return (data_frame(media = media, varianza = varianza))
      
    })
  })
  
  graficaGamma <- function() {
    #inputs
    alpha <- input$alpha
    if(!isNumericGreatThan0(alpha)) {
      showNotification("Alpha debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      alpha <- 1
    }
    theta <- input$theta
    if(!isNumericGreatThan0(theta)) {
      showNotification("Theta debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      theta <- 1
    }
    
    media <- alpha*theta#cambiar operacion segun el parametro
    
    #inputs
    desviacion <- sqrt(alpha)*theta #cambiar segun el parametro
    muestra <- 10000
    amplitud <- 7 * desviacion
    
    xInit = media - amplitud
    if(xInit <0) {
      xInit <- -0.5
    }
    xEnd = media + amplitud
    
    #contentido
    xValues <- seq(xInit, xEnd, length = muestra)
    yValues <- dgamma(xValues, shape = alpha, scale = theta)
    yValorAcumulado <- pgamma(xValues, shape = alpha, scale = theta)
    
    mostrarGraficaConValores(xInit, xValues, yValues, yValorAcumulado)
  }
  
  graficaAcumuladaGamma <- function() {
    #inputs
    alpha <- input$alpha
    if(!isNumericGreatThan0(alpha)) {
      showNotification("Alpha debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      alpha <- 1
    }
    theta <- input$theta
    if(!isNumericGreatThan0(theta)) {
      showNotification("Theta debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      theta <- 1
    }
    
    media <- alpha*theta#cambiar operacion segun el parametro
    
    ## Contenido
    x <- seq(from  = round(qgamma(.01, shape = alpha, scale = theta),0)-.5, 
              to = round(qgamma(.99, shape = alpha, scale = theta),0)+3, 
              length.out = 1000)
    
    ## Función de densidad acumulada:
    curve(pgamma(x, shape = alpha, scale = theta), xlab="", ylab="", from = x[1], to = x[1000], col="purple", lwd=2, main="Función de Distribución Acumulada")
  }
  
  output$optionsGamma <- renderUI({
    return( tagList(numericInput(ns("alpha"), "Alpha", value = 1, min = 0.0000000001,  step = 0.1),
                    numericInput(ns("theta"), "Theta", value = 1, min =0.0000000001,  step = 0.1)))
  })
  
  #ji2
  observeEvent(input$probji2, {
    tema$seleccion <- "Ji-Cuadrada"
    tema$discreta <- FALSE
    output$'SummaryJi-Cuadrada' <- renderTable({
      freedom <- input$jiN
      
      ## Estadisticos
     return (data_frame(minimo = qchisq(0, df = freedom ),
                            primer_cuartil = qchisq(.25,df = freedom),
                            mediana = qchisq(.5,df = freedom),
                            tercer_cuartil = qchisq(.75, df = freedom),
                            maximo = qchisq(1, df = freedom)))
    })
    
    output$'EstadisticosJi-Cuadrada' <- renderTable({
      freedom <- input$jiN
      ## Media de la muestra:
      media <- freedom#cambiar operacion segun el parametro
      
      varianza <- 2*freedom
      
      return (data_frame(media = media, varianza = varianza))
      
    })
  })
  
  'graficaJi-Cuadrada' <- function() {
    #inputs
    freedom <- input$jiN
    #inputs
    if(!isNumericGreatThan0(freedom)) {
      showNotification("Alpha debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      alpha <- 1
    }
    
    media <- freedom#cambiar operacion segun el parametro
    
    #inputs
    desviacion <- 2*freedom #cambiar segun el parametro
    muestra <- 10000
    amplitud <- 7 * desviacion
    
    xInit = media - amplitud
    if(xInit <0) {
      xInit <- -0.5
    }
    xEnd = media + amplitud
    
    #contentido
    xValues <- seq(xInit, xEnd, length = muestra)
    yValues <- dchisq(xValues,df =freedom)
    yValorAcumulado <- pchisq(xValues, df =freedom)
    
    mostrarGraficaConValores(xInit, xValues, yValues, yValorAcumulado)
  }
  
  'graficaAcumuladaJi-Cuadrada' <- function() {
    freedom <- input$jiN
    #inputs
    if(!isNumericGreatThan0(freedom)) {
      showNotification("Alpha debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      freedom <- 1
    }
    
    ## Contenido
    x <- seq(from  = round(qchisq(.01, df = freedom),0)-.5, 
             to = round(qchisq(.99,df = freedom),0)+3, 
             length.out = 1000)
                        
    
    ## Función de densidad acumulada:
    curve(pchisq(x, df =freedom), xlab="", ylab="", from = x[1], to = x[1000], col="purple", lwd=2, main="Función de Distribución Acumulada")
  }
  
  output$'optionsJi-Cuadrada' <- renderUI({
    return( numericInput(ns("jiN"), "n", value = 1, min = 1,  step = 1))
  })
  
  #T
  observeEvent(input$probT, {
    tema$seleccion <- "T"
    tema$discreta <- FALSE
    output$SummaryT <- renderTable({
      df <- input$gradosLibertad
      
      ## Estadisticos
     return (data_frame(percentil_75 = qt(.75, df),
                        percentil_90 = qt(.90, df),
                        percentil_95 = qt(.95, df),
                        percentil_97.5 = qt(.975, df),
                        percentil_99 = qt(.99, df),
                        percentil_99.5 = qt(.995, df),
                        percentil_100 = qt(1, df)))
    })
    
    
  })
  
  graficaT <- function() {
    df <- input$gradosLibertad#grados de libertad
    if(!isNumericGreatThan0(df)) {
      showNotification("Alpha debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      df <- 5
    }
    ## Contenido
    xValues <- unique(seq(from  = qt(.0001, df, log = FALSE), 
                          to = qt(.9999, df, log = FALSE), 
                          length.out = 1000))
    xInit <-  min(xValues)
    yValues <- dt(xValues, df, log = FALSE)
    yValorAcumulado <- pt(xValues, df, log = FALSE)
    ## Distribucion de probabilidad
    mostrarGraficaConValores(xInit, xValues, yValues, yValorAcumulado)
  }
  
  graficaAcumuladaT <- function() {
    df <- input$gradosLibertad#grados de libertad
    if(!isNumericGreatThan0(df)) {
      showNotification("Alpha debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      df <- 5
    }
    ## Contenido
    x <- seq( from  = round(qt(.01, df),0), 
              to = round(qt(.99, df),1), 
              length.out = 1000)
    
    ## Función de densidad acumulada:
    curve(pt(x, df), xlab="", ylab="", from = x[1], to = x[1000], col="purple", lwd=2, main="Función de Distribución Acumulada")
  }
  
  output$optionsT <- renderUI({
    return( numericInput(ns("gradosLibertad"), "n", value = 3, min = 1,  step = 1))
  })
  
  #F
  observeEvent(input$probF, {
    tema$seleccion <- "F"
    tema$discreta <- FALSE
    output$SummaryF <- renderTable({
      df1 <- input$GL1
      df2 <- input$GL2
      
      ## Estadisticos
      return (data_frame(percentil_75 = qf(.75,df1, df2, log = FALSE),
                         percentil_90 = qf(.90,df1, df2, log = FALSE),
                         percentil_95 = qf(.95, df1, df2, log = FALSE),
                         percentil_97.5 = qf(.975, df1, df2, log = FALSE),
                         percentil_99 = qf(.99,df1, df2, log = FALSE),
                         percentil_99.5 = qf(.995, df1, df2, log = FALSE),
                         percentil_100 = qf(1, df1, df2, log = FALSE)))
    })
  })
  
  graficaF <- function() {
    df1 <- input$GL1#grados de libertad
    df2 <- input$GL2#grados de libertad
    if(!isNumericGreatThan0(df1)) {
      showNotification("n debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      df1 <- 1
    }
    if(!isNumericGreatThan0(df2)) {
      showNotification("m debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      df2 <- 1
    }
    ## Contenido
    xValues <- unique(seq(from  = qf(.0001, df1, df2, log = FALSE), 
                          to = qf(.9999, df1, df2, log = FALSE), 
                          length.out = 1000))
    xInit <-  min(xValues)
    yValues <- df(xValues, df1, df2, log = FALSE)
    yValorAcumulado <- pf(xValues, df1, df2, log = FALSE)
    ## Distribucion de probabilidad
    mostrarGraficaConValores(xInit, xValues, yValues, yValorAcumulado)
  }
  
  graficaAcumuladaF <- function() {
    df1 <- input$GL1#grados de libertad
    df2 <- input$GL2#grados de libertad
    if(!isNumericGreatThan0(df1)) {
      showNotification("n debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      df1 <- 1
    }
    if(!isNumericGreatThan0(df2)) {
      showNotification("m debe ser un numero válido mayor que 0",closeButton = TRUE, type = "error")
      df2 <- 1
    }
    ## Contenido
    x <- unique(seq(from  = qf(.0001, df1, df2, log = FALSE), 
                          to = qf(.9999, df1, df2, log = FALSE), 
                          length.out = 1000))
    ## Función de densidad acumulada:
    curve(pf(x, df1, df2, log = FALSE), xlab="", ylab="", from = x[1], to = x[1000], col="purple", lwd=2, main="Función de Distribución Acumulada")
  }
  
  output$optionsF <- renderUI({
    return( tagList(numericInput(ns("GL1"), "m", value = 15, min = 1,  step = 1),
                    numericInput(ns("GL2"), "n", value = 13, min = 1,  step = 1)))
  })
  
  
  mostrarGraficaConValores <- function(xInit, xValues, yValues, yValorAcumulado) {
    colorCaclularX <- "#7f00ff"
    colorCalcularA <- "#0080FF"
    if(input$calculate == "ValorX") {
        probabilidad <- input$probabilidadAcumulada
        if(!isNumericGreatThan0(probabilidad) | probabilidad >1) {
          showNotification("la probabilidad solo puede ser entre 0 y 1",closeButton = TRUE, type = "error")
          probabilidad <- 0.5
        }
        fillArea <- probabilidad >= yValorAcumulado
      
        x_within_bounds <- xValues[fillArea]
        y_within_bounds <- yValues[fillArea]
        
        index <- min(which(fillArea == FALSE))
        if(!is.null(input$limSuperior)) {
          if(tema$seleccion == "Uniforme" & input$probabilidadAcumulada >=1) {
            index <- length(xValues)
          }
        }
        
        endXvalue <- xValues[index]
        x_polygon <- c(xInit, x_within_bounds, endXvalue)
        y_polygon <- c(0, y_within_bounds, 0)
        
        textlabel <-sprintf("x = %.4f", round(endXvalue,4))
        plot(xValues, yValues, type="line", xlab = textlabel, ylab = "", main="Función de Densidad")
        polygon(x_polygon, y_polygon, col = colorCaclularX)
      } else {
        endXvalue <- as.numeric(input$xValue)
        if(!is.null(input$limSuperior)) {
          if(tema$seleccion == "Uniforme" && endXvalue > as.numeric(input$limSuperior)) {
            endXvalue <- input$limSuperior
            yValorAcumulado[length(yValorAcumulado)] <- 1
          }
        }
        fillArea <- xValues <= endXvalue
        
        x_within_bounds <- xValues[fillArea]
        y_within_bounds <- yValues[fillArea]
        
        x_polygon <- c(xInit, x_within_bounds, endXvalue)
        y_polygon <- c(0, y_within_bounds, 0)
        
        index <- min(which(fillArea == FALSE))
        valorAcumulado <- NULL
        if(!is.null(input$limSuperior)) {
          if(tema$seleccion == "Uniforme" & endXvalue >= as.numeric(input$limSuperior)) {
            valorAcumulado <- 1
          } else {
            valorAcumulado <- yValorAcumulado[index]
          }
        } else {
          valorAcumulado <- yValorAcumulado[index]
        }
       
        textlabel <- sprintf("probabilidad acumulada = %f", valorAcumulado)
        plot(as.numeric(xValues), yValues, type="line", xlab = textlabel, ylab = "",main="Función de Densidad")
        polygon(x_polygon, y_polygon, col = colorCalcularA)
      }
    abline(h = 0, col = "black", lwd = 1)
  }
  
  mostrarSelectorDeCalculo <- function() {
     tagList(radioButtons(ns("calculate"), 
              label = HTML(paste("P ( X ≤ x ) = a","Calcular :" , sep="<br/>")),
              choices = c( x = "ValorX",a = "ProbaAcum"),
              selected = "ValorX"),
          conditionalPanel( condition = "input['distribuciones-calculate'] == 'ProbaAcum'",
                             textInput(ns("xValue"), "x = ", 100)),
          conditionalPanel( condition = "input['distribuciones-calculate'] == 'ValorX'",
                             numericInput(ns("probabilidadAcumulada"), "a = ", 0.5,  min = 0, max = 0.99999999999, step = .01)),
          plotOutput(ns("mostrarGrafica")))
  }
  
  output$mostrarGrafica <- renderPlot({
    eval(call(sprintf("grafica%s",tema$seleccion)))
  })
  
  output$mostrarGraficaAcumulada <- renderPlot({
    eval(call(sprintf("graficaAcumulada%s",tema$seleccion)))
  })
  
  output$graficaProbabilidades <- renderUI({
    if(tema$discreta) {
      tagList(
        plotOutput(ns(sprintf("mostrarGrafica"))),
        downloadButton(ns('ImageSave'), 'Exportar Imagen')
      )
    }else {
      return (tagList(
        mostrarSelectorDeCalculo(),
        downloadButton(ns('ImageSave'), 'Exportar Imagen')
      ))
    }
  })
  
  output$graficaAcumulada <- renderUI({
    return (tagList(
      plotOutput(ns(sprintf("mostrarGraficaAcumulada"))),
      downloadButton(ns('ImageSaveAcumulada'), 'Exportar Imagen')
    ))
  })
  
  #export images
  output$ImageSave <- downloadHandler(
    filename = sprintf("%s.%s", tema$seleccion, "png"),
    content = function(file) {
      png(file)
      eval(call(sprintf("grafica%s",tema$seleccion)))
      dev.off()
    }, 
    contentType = "image/png") 
 
   
  output$ImageSaveAcumulada <- downloadHandler(
    filename = sprintf("Acumulada%s.%s", tema$seleccion, "png"),
    content = function(file) {
      png(file)
      eval(call(sprintf("graficaAcumulada%s",tema$seleccion)))
      dev.off()
    }, 
    contentType = "image/png") 
  
  #frame base que muestra todo
  output$FrameGeneral <- renderUI({
    if(is.null(tema$seleccion)) {
      return(tags$h3(style = "text-align: center;","Selecciona una distribución"))
    }
    tagList(
              tags$div(style="display: inline-block", h2(tema$seleccion)),
              tags$div(style="display: inline-block; padding-left: 30px", 
                       uiOutput(ns("formula"))
            ),
            uiOutput(ns(sprintf("options%s",tema$seleccion))) ,
            uiOutput(ns("tabPanel")))
  })
  
  output$formula <- renderUI({
    return(tags$img(src = sprintf("/images/%s.png", tema$seleccion)))
  })
  
  isNumeric <- function(x) is.numeric(x) & !is.na(x)
  
  isNumericGreatThan0 <- function(x) is.numeric(x) & !is.na(x) & x>0
}
