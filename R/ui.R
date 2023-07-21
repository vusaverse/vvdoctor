# Shiny App UI
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
        shiny::fileInput("file", "Upload a file"),
        shiny::selectInput("dependent_var", "Choose Dependent Variable",
                           choices = NULL),
        shiny::selectInput("independent_var", "Choose Independent Variable",
                           choices = c("None", "continuous", "categorical")),
        shiny::numericInput("mean_input", "Enter Mean:", value = 0, step = 0.1,
                            min = -999, max = 999)
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
                        shiny::verbatimTextOutput("dataView"),  # Display data info
                        shiny::verbatimTextOutput("dependent_var_info")  # Display dependent variable info
          )
        )
      )
    )
  )
}
