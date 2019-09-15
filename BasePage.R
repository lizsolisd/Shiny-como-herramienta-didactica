#Base Page
library(shiny)
library(shinydashboard)

source("Distribuciones.R")
source("EstadisticaDescriptiva.R")
source("PruebaHipotesis.R")
source("Conteo.R")

mostrarTituloYTemas <- function(id, label = "base") {
  ns <- NS(id)
    dashboardPage(
      dashboardHeader(title = "Estadística"),
      dashboardSidebar(
      #combo box con los temas principales
      selectInput(inputId = ns("temario"), "", c(
        "Conteo",
        "Distribuciones de Probabilidad",
        "Estadística Descriptiva",
        "Prueba de Hipotesis")),
        conditionalPanel(
            condition = "input['base-temario'] == 'Distribuciones de Probabilidad'",
            renderDistribucionesOptions("distribuciones")),
        conditionalPanel(
            condition = "input['base-temario'] == 'Estadística Descriptiva'",
            renderBaseTableOption()),
        conditionalPanel(
            condition = "input['base-temario'] == 'Prueba de Hipotesis'",
            mostrarHipotesisOptions())
      ), 
      dashboardBody(
        conditionalPanel(
          condition = "input['base-temario'] == 'Distribuciones de Probabilidad'",
          renderDistribuciones("distribuciones")),
        conditionalPanel(
          condition = "input['base-temario'] == 'Estadística Descriptiva'",
          renderTableUI()),
        conditionalPanel(
          condition = "input['base-temario'] == 'Conteo'",
          mostrarPantallaConteo()),
        conditionalPanel(
          condition = "input['base-temario'] == 'Prueba de Hipotesis'",
          mostrarPantallaHipotesis())
      ))
}