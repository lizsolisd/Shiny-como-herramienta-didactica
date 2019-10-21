## Instalación de paquetes en las versiones requeridas.
if("devtools" %in% rownames(installed.packages()) == FALSE) {
  install.packages("devtools")
} else cat("\ndevtools already installed\n")

library(devtools)

if("readr" %in% rownames(installed.packages()) == FALSE) {
  install_version("readr", version = "1.3.1")
}else cat("\nreadr already installed\n")

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install_version("dplyr", version = "0.8.0.1")
}else cat("\ndplyr already installed\n")

if("rhandsontable" %in% rownames(installed.packages()) == FALSE) {
  install_version("rhandsontable", version = "0.3.7")
}else cat("\nrhandsontable already installed\n")

if("rpivotTable" %in% rownames(installed.packages()) == FALSE) {
  install_version("rpivotTable", version = "0.3.0")
}else cat("\nrpivotTable already installed\n")

if("knitr" %in% rownames(installed.packages()) == FALSE) {
  install_version("knitr", version = "1.22")
}else cat("\nknitr already installed\n")

if("combinat" %in% rownames(installed.packages()) == FALSE) {
  install_version("combinat", version = "0.0-8")
}else cat("\ncombinat already installed\n")

if("shiny" %in% rownames(installed.packages()) == FALSE) {
  install_version("shiny", version = "1.3.2")
}else cat("\nshiny already installed\n")

if("shinydashboard" %in% rownames(installed.packages()) == FALSE) {
  install_version("shinydashboard", version = "0.7.1")
}else cat("\nshinydashboard already installed\n")

