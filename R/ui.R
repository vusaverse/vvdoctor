#' Shiny App UI
#'
#' This file defines the UI layout for the Shiny app.
#'
#' @export
app_ui <- function() {
  shiny::fluidPage(
    # Application title
    shiny::titlePanel("Statistical Test App"),

    # Use the shinyjs library
    shinyjs::useShinyjs(),

    # Sidebar with a browse button for file upload
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload a file")
      ),

      # Show a plot of the uploaded data (Histogram for continuous variables)
      shiny::mainPanel(
        shiny::plotOutput("distPlot"),
        shiny::verbatimTextOutput("dataView"),  # Display data info
        DT::dataTableOutput("dataTable"),

        # Dropdown to select the class of the independent variable
        shiny::selectInput("independent_var", "Independent Variable Class",
                           choices = c("continuous", "categorical")),

        # Dropdown to select the number of groups for the dependent variable
        shiny::selectInput("dependent_var", "Number of Dependent Variable Groups",
                           choices = c(1, 2, ">2")),

        # Input field to enter the mean for the independent variable (hidden by default)
        shiny::numericInput("mean_input", "Enter Mean:", value = 0, step = 0.1,
                            min = -999, max = 999),

        # Display the recommended statistical test based on the chosen variables
        shiny::textOutput("recommended_test")
      )
    )
  )
}
