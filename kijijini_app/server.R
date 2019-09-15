library(shiny)
if (Sys.getlocale()!="en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/C"){
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
}

source("Distribuciones.R")
source("EstadisticaDescriptiva.R")
source("PruebaHipotesis.R")
source("Conteo.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  callModule(setupDistribucionesListeners,"distribuciones")
  callModule(setupDescriptivaListeners,"leerArchivo")
  callModule(setupHipotesisListeners, "hipotesis")
  callModule(setupConteoListeners, "conteo")
  
})  

#to display the code in the app
#shiny::runApp(paste(getwd(),"/tesis_aplicacion",sep = ""), display.mode =
#"showcase")