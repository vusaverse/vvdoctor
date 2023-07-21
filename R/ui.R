#' Shiny App UI
#'
#' This file defines the UI layout for the Shiny app.
#'
#' @export
app_ui <- function() {
  shiny::fluidPage(
    # Application title
    shiny::titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a browse button for file upload
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload a file")
      ),

      # Show a plot of the uploaded data
      shiny::mainPanel(
        shiny::plotOutput("distPlot")
      )
    )
  )
}
