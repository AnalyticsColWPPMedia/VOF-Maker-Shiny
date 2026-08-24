# global.R

# 1. Cargar Librerías
library(shiny)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(readxl)
library(glue)
library(shinyjs)
library(jsonlite)
library(shinyWidgets)

# 2. Cargar Funciones y Módulos
source("R/core/utils.R")
source("R/modules/mod_variable_row.R")

# 3. Configuraciones Globales (si hubiera constantes, irían aquí)