#Base Page
library(shiny)
library(shinydashboard)

source("C_Distribuciones.R")
source("B_EstadisticaDescriptiva.R")
source("D_PruebaHipotesis.R")
source("A_Conteo.R")

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