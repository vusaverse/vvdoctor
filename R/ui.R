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
        # Place elements in a fluidRow
        shiny::fluidRow(
          shiny::fileInput("file", "Upload a file"),
          # Dropdown to select the class of the independent variable
          shiny::selectInput("independent_var", "Independent Variable Class",
                             choices = c("continuous", "categorical")),
          # Dropdown to select the number of groups for the dependent variable
          shiny::selectInput("dependent_var", "Number of Dependent Variable Groups",
                             choices = c(1, 2, ">2")),
          # Input field to enter the mean for the independent variable (hidden by default)
          shiny::numericInput("mean_input", "Enter Mean:", value = 0, step = 0.1,
                              min = -999, max = 999)
        ),
        # Display the recommended statistical test right below the dropdowns
        shiny::textOutput("recommended_test"),
        # Display info about the dependent variable based on class and distribution
        shiny::textOutput("dependent_var_info")
      ),

      # Show a plot of the uploaded data (Histogram for continuous variables)
      shiny::mainPanel(
        # Place the table and the rest of the elements in a fluidRow
        shiny::fluidRow(
          # Place the table in a column that takes up 6 out of 12 units
          shiny::column(width = 6, DT::dataTableOutput("dataTable")),
          # Place the histogram plot and other elements in a column that takes up 6 out of 12 units
          shiny::column(width = 6,
                        # Display the histogram only when a continuous variable is selected
                        shiny::conditionalPanel(
                          condition = "input.independent_var == 'continuous'",
                          shiny::plotOutput("histPlot")
                        ),
                        shiny::verbatimTextOutput("dataView")  # Display data info
          )
        )
      )
    )
  )
}
