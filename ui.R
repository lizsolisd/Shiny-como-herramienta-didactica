library(shiny)
library(dplyr)
source("BasePage.R")

# Define UI for application thast draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("h1 {
                     font-family: 'Trebuchet MS', normal;
                     font-weight: 600;
                     line-height: 1.1;
                     color: #3366ff;
                    }
                    h2 {
                     font-family: 'Trebuchet MS', normal;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #3366ff;
                    }
                    body {
                      background-color: #ccefff;
                    }
                    li {
                      background-color: #eeeeee;
                    }
                    .table > thead > tr > th,.table > tbody > tr > th,
                    .table > tfoot > tr > th,.table > thead > tr > td,
                    .table > tbody > tr > td,.table > tfoot > tr > td {
                      line-height: 1.42857143;
                      vertical-align: top;
                      border-top: 1px solid #333;
                      border-bottom: 1px solid #333;
                      border-left: 2px solid #333;
                      border-right: 2px solid #333;
                    }
                    .table > thead > tr > th {
                      vertical-align: bottom;
                      border: 2px solid #333;
                    }
                    .table > caption + thead > tr:first-child > th,
                    .table > colgroup + thead > tr:first-child > th,
                    .table > thead:first-child > tr:first-child > th,
                    .table > caption + thead > tr:first-child > td,
                    .table > colgroup + thead > tr:first-child > td,
                    .table > thead:first-child > tr:first-child > td {
                      border-top: 2px solid;
                    }
                    table {
                      border-bottom : 2px solid #333;
                    }
                    .form-group {
                      margin-bottom:5px;
                    }
                    .content-wrapper{
                      background-color: #ccefff;
                    }
                    .skin-blue .wrapper{
                      background-color: #ccefff;
                    }
                    section.sidebar .shiny-input-container {
                      padding-top: 3px;
                      padding-right: 3px;
                      padding-bottom: 0px;
                      padding-left: 10px;
                    }
                    .progress{
                      margin-bottom:3px;
                    }
                    .checkbox {
                      margin-bottom:3px;
                      margin-top:3px;
                    }
                    "))
    ),
  
  mostrarTituloYTemas("base")
  
))