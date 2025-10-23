library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinycssloaders)

library(DT)
library(purrr)
library(rstan)
library(data.table)
library(dplyr)
library(stats)
library(R6)

library(ggplot2)
library(gridExtra)
library(gridGraphics)
library(plotly)

#plan(multisession)

gc()
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(encoding = "UTF-8")
options(shiny.maxRequestSize = 100*1024^2) 

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


source('modulos/funciones.R')
source('modulos/funcionesTest.R')
source('modulos/FunDiagnosticoPosterior.R')
source('modulos/FuncionModeloPosterior.R')
source('modulos/ModulosUI.R')
source('modulos/ModulosServer.R')
source('modulos/ElicitarModuloSolo.R')
#source('modulos/ui.R')
#source('modulos/server.R')





