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

        # Dropdown for choosing the independent variable
        shiny::uiOutput("independent_var_dropdown"),

        # Input field for mean (hidden initially)
        shiny::textInput("input_mean", "Input mean:", value = ""),

        # Text below the dropdowns
        shiny::textOutput("dependent_var_text")
      ),

      # Show the datatable and histogram after submitting a file
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(width = 12, DT::dataTableOutput("dataTable")),
          shiny::column(width = 12, plotOutput("dependent_var_histogram"))
        )
      )
    )
  )
}
