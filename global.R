# global.R
# Packages, global configuration and sourcing of the business logic (R/core)
# and Shiny modules (R/modules).
#
# The app is written with the tidyverse (team standard). Only the VOF code that
# the app GENERATES must be base R, because AWB runs it on R 3.6.

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(readxl)
library(lubridate)
# jsonlite, plotly and DT are used with pkg:: calls (attaching jsonlite would
# mask shiny::validate; plotly would mask dplyr verbs such as filter()).

# Files are sourced in name order: core files are numbered by dependency.
for (f in sort(list.files("R/core", pattern = "\\.R$", full.names = TRUE))) source(f, local = FALSE)
for (f in sort(list.files("R/modules", pattern = "\\.R$", full.names = TRUE))) source(f, local = FALSE)

APP_TITLE <- "VOF Maker"
APP_SUBTITLE <- "By Advanced Analytics Colombia"
