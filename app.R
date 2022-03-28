# Load libraries
library("shiny")
library("ggplot2")
library("plotly")
library("leaflet")
library("dplyr")
library("tidyverse")

# Get sources for ui and server
source("app_ui.R")
source("app_server.R")

shinyApp(ui = ui, server = server)
