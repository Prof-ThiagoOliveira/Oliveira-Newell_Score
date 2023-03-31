########################################################################
## Project: Oliveira-Newell Score
## Script purpose: Run Shiny App
## Date: 2023-03-31
## Author: Thiago de Paula Oliveira
########################################################################
source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)
