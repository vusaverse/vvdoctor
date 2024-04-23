# app.R

# Load required packages
library(shiny)

# Source UI and server functions
source("R/ui.R")
source("R/global.R")
source("R/server.R")
# source("R/helperfunctions.R")

# Define run_vvdoctor() function
run_vvdoctor <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server)
}

# Run the app
run_vvdoctor()
