#' App UI
#'
#' @return A Shiny UI object
#' @export
#'
app_ui <- function() {
  shiny::fluidPage(
    rintrojs::introjsUI(),
    # Application title
    shiny::titlePanel(shiny::tags$h3("vvdoctor: The App for Statistical Testing", windowTitle = "vvdoctor: The App for Statistical Testing")),
    shiny::titlePanel(shiny::tags$h5(
      shiny::tags$a("Our blog",href="https://vusaverse.github.io/posts/vvdoctor_alpha_release.html")
    )
    ),
    # Use the shinyjs library
    shinyjs::useShinyjs(),

    # Sidebar with a browse button for file upload anid the "About this app" action button
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tags$h4("Step 1: Upload Data"),
        shiny::actionButton("launch_modal", "Import data"),
        shiny::actionButton("about_app", "About this app"),
        shiny::tags$hr(),
        shiny::tags$h4("Step 2: Select Variables"),
        rintrojs::introBox(
          shiny::uiOutput("dependent_var_dropdown"),
          data.step = 3,
          data.intro = "Choose the dependent variable from this dropdown."
        ),
        rintrojs::introBox(
          shiny::textOutput("dependent_var_text"),
          data.step = 4,
          data.intro = "This is the dependent variable text."
        ),
        rintrojs::introBox(
          shiny::uiOutput("independent_var_dropdown"),
          data.step = 5,
          data.intro = "Choose the independent variable from this dropdown."
        ),
        rintrojs::introBox(
          shiny::textOutput("independent_var_text"),
          data.step = 6,
          data.intro = "This is the independent variable text."
        ),
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
        shiny::tags$hr(),
        shiny::tags$h4("Step 3: Choose Test"),
        rintrojs::introBox(
          shiny::uiOutput("statistical_test_dropdown"),
          data.step = 9,
          data.intro = "Choose the statistical test from this dropdown."
        ),
        shiny::tags$hr(),
        shiny::tags$h4("Step 4: View Results"),
        rintrojs::introBox(
          shiny::verbatimTextOutput("test_report"),
          data.step = 10,
          data.intro = "This is the area to display the test report."
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          tabPanel("Data Preview",
            DT::dataTableOutput("dataTable")
          ),
          tabPanel("Variable Analysis",
            shiny::plotOutput("dependent_var_histogram")
          ),
          tabPanel("Test Results",
            shiny::verbatimTextOutput("test_report")
          ),
          tabPanel("Interpretation",
            shiny::textOutput("interpretation_text") # Placeholder for interpretation guidance
          )
        )
      )
    )
  )
}
