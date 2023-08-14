#' App UI
#'
#' @export
#'
app_ui <- function() {
  shiny::fluidPage(
    # Application title
    shiny::titlePanel("Statistical Test App"),

    # Use the shinyjs library
    shinyjs::useShinyjs(),

    # Sidebar with a browse button for file upload
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload a file"),

        # Dropdown for choosing the dependent variable
        shiny::uiOutput("dependent_var_dropdown"),

        # Text below the dropdowns
        shiny::textOutput("dependent_var_text"),

        # Radio button for choosing paired or unpaired
        shiny::radioButtons("paired_unpaired", "Choose paired or unpaired:", choices = c("Paired", "Unpaired")),

        # Dropdown for choosing the independent variable
        shiny::uiOutput("independent_var_dropdown"),

        shiny::textOutput("independent_var_text"),  # New output element for the independent variable text

        # Input field for mean (hidden initially)
        shiny::uiOutput("input_mean"),

        # New dropdown for selecting statistical test
        shiny::uiOutput("statistical_test_dropdown"),

        # Area to display the test report
        shiny::verbatimTextOutput("test_report")
      ),

      # Show the datatable and histogram after submitting a file
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(width = 12, DT::dataTableOutput("dataTable")),
          shiny::column(width = 12, shiny::plotOutput("dependent_var_histogram"))
        )
      )
    )
  )
}
