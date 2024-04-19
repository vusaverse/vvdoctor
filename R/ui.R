#' App UI
#'
#' @export
#'
app_ui <- function() {
  shiny::fluidPage(

    rintrojs::introjsUI(),
    # Application title
    shiny::titlePanel("vvdoctor statistics app"),

    # Use the shinyjs library
    shinyjs::useShinyjs(),

    # Sidebar with a browse button for file upload anid the "About this app" action button
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # shiny::fileInput("file", "Upload a file"),


        shiny::actionButton("launch_modal", "Import data"),

        # Action button for displaying the info panels
        shiny::actionButton("about_app", "About this app"),

        # Dropdown for choosing the dependent variable
        rintrojs::introBox(
          shiny::uiOutput("dependent_var_dropdown"),
          data.step = 3,
          data.intro = "Choose the dependent variable from this dropdown."
        ),

        # Text below the dropdowns
        rintrojs::introBox(
          shiny::textOutput("dependent_var_text"),
          data.step = 4,
          data.intro = "This is the dependent variable text."
        ),

        # Dropdown for choosing the independent variable
        rintrojs::introBox(
          shiny::uiOutput("independent_var_dropdown"),
          data.step = 5,
          data.intro = "Choose the independent variable from this dropdown."
        ),
        rintrojs::introBox(
          shiny::textOutput("independent_var_text"), # New output element for the independent variable text
          data.step = 6,
          data.intro = "This is the independent variable text."
        ),

        # Input field for mean (hidden initially)
        rintrojs::introBox(
          shiny::uiOutput("input_mean"),
          data.step = 7,
          data.intro = "Enter the mean here."
        ),
        shiny::conditionalPanel(
          condition = "input.statistical_test == 'Repeated measures ANOVA (paired)' || input.statistical_test == 'Multilevel Logistic Regression (paired)' || input.statistical_test == 'Friedman\\'s ANOVA II (paired)'",
          rintrojs::introBox(
            shiny::uiOutput("identifier_dropdown"),
            data.step = 8,
            data.intro = "Choose the identifier from this dropdown."
          )
        ),

        # New dropdown for selecting statistical test
        rintrojs::introBox(
          shiny::uiOutput("statistical_test_dropdown"),
          data.step = 9,
          data.intro = "Choose the statistical test from this dropdown."
        ),

        # Area to display the test report
        rintrojs::introBox(
          shiny::verbatimTextOutput("test_report"),
          data.step = 10,
          data.intro = "This is the area to display the test report."
        )
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
