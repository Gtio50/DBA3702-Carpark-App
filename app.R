# Library -----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)

# Sources -----------------------------------------------------------------
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
source("R/functions.R")
source("R/server.R")
source("R/ui.R")
source("R/data manager.R")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  yhMapUI("Map1")
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  yhMapServer("Map1")
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)