#' Shiny App UI
#'
#' This file defines the UI layout for the Shiny app.
#'
#' @export
app_ui <- function() {
  shiny::fluidPage(
    # Application title
    shiny::titlePanel("Statistical Test App"),

    # Sidebar with a browse button for file upload
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload a file"),
        shiny::selectInput("independent_var", "Select Independent Variable", NULL),
        shiny::selectInput("dependent_var", "Select Dependent Variable", NULL),
        shiny::uiOutput("variable_class_info"),  # Display the variable class info here
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
