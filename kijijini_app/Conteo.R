library(combinat)
library(parallel)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
parLapply(cl, 2:1000,function(exponent)2^exponent)
#Conteo
mostrarPantallaConteo <- function(id= "conteo", label = "conteo") {
  ns <- NS(id)
  uiOutput(ns("FrameGeneral"))
}

setupConteoListeners <- function(input, output, session, label = "conteo") {
   ns <- session$ns
   
   output$FrameGeneral <- renderUI({
     tagList(h2("Conteo"),
             tags$div(style= "display: inline-block; width:150px",numericInput(ns("N"), "n = ", 5,  min = 0, step = 1)),
             tags$div(style= "display: inline-block; width:150px",numericInput(ns("X"), "x = ", 2, min= 0, step = 1)),
             tabsetPanel(tabPanel("Combinaciones",uiOutput(ns("renderCombinaciones"))),
                         tabPanel("Permutaciones",uiOutput(ns("renderPermutaciones"))), 
                         tabPanel("Ordenaciones",uiOutput(ns("renderOrdenaciones"))))
      )
   })
   
   output$renderPermutaciones <- renderUI({
     result <-  (choose(input$N,input$X)*factorial(input$X))
     return (tags$div( style = "text-align: center;",
                       tags$br(),
                       tags$br(),
                       tags$img(style = "display: block;margin-left: auto;margin-right: auto", 
                                src = "/images/Permutaciones.png"),
                       tags$br(),
                       h4(sprintf("%s P %s=  %s!/(%s-%s)! = %s",
                           input$N, input$X, input$N,input$N,input$X , result))
     ))
   })
   
   output$renderCombinaciones <- renderUI({
     result <-  choose(input$N, input$X)
     return (tags$div( style = "text-align: center;",
                       tags$br(),
                       tags$br(),
                       tags$img(style = "display: block;margin-left: auto;margin-right: auto", 
                                src = "/images/Combinaciones.png"),
                       tags$br(),
                       h4(sprintf("%s C %s = %s! / [(%s - %s)! %s!] = %s",
                           input$N, input$X,input$N,input$N,input$X,input$X, result))
     ))
   })
   output$renderOrdenaciones <- renderUI({
     result <- input$N ^ input$X
     return (tags$div( style = "text-align: center;",
                       tags$br(),
                       tags$br(),
                       tags$img(style = "display: block;margin-left: auto;margin-right: auto", 
                                src = "/images/Orden.png"),
                       tags$br(),
                       h4(sprintf("%s O %s = %s ^ %s  = %s",
                           input$N, input$X, input$N, input$X, result ))
     ))
   })
   
}