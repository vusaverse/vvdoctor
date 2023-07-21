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
        shiny::fileInput("file", "Upload a file"),
        shiny::selectInput("col_select_1", "Select Column 1", NULL),
        shiny::selectInput("col_select_2", "Select Column 2", NULL),
        shiny::uiOutput("mean_input")  # Display the input field here
      ),

      # Show a plot of the uploaded data
      shiny::mainPanel(
        shiny::plotOutput("distPlot"),
        shiny::verbatimTextOutput("dataView"),  # Dedicated "View" pane
        DT::dataTableOutput("dataTable")
      )
    )
  )
}
