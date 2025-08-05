#' App UI
#'
#' @return A Shiny UI object
#' @export
#'
app_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(HTML('
        /* VU Analytics branding colors and accessible contrast */
        body { background-color: #f5f5f5; }
        .sidebarPanel { background-color: #002d72; color: #ffffff; }
        .mainPanel { background-color: #ffffff; color: #222222; overflow-x: auto; }
        .btn, .shiny-input-container .action-button { background-color: #ffc72c; color: #002d72; border: none; }
        .btn:hover, .shiny-input-container .action-button:hover { background-color: #002d72; color: #ffc72c; }
        .tabset-panel > .nav-tabs > li.active > a { background-color: #ffc72c !important; color: #002d72 !important; }
        .tabset-panel > .nav-tabs > li > a { color: #002d72 !important; }
        .shiny-output-error { color: #d32f2f; background: #fff3e0; padding: 8px; border-radius: 4px; }
        .shiny-input-container { width: 100% !important; }
        h4, h3, h5, label, .form-label { color: #002d72 !important; background: #fff !important; }
        .form-control, .picker { background: #fff !important; color: #222 !important; }
        @media (max-width: 768px) {
          .sidebarPanel, .mainPanel { width: 100% !important; float: none !important; }
        }
        /* Focus indicators for accessibility */
        button:focus, .action-button:focus, .btn:focus, select:focus, input:focus, .shiny-input-container:focus {
          outline: 3px solid #ffc72c !important;
          outline-offset: 2px;
          box-shadow: 0 0 0 2px #002d72;
        }
      '))
    ),
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
        shiny::tags$h4(list(shiny::icon("upload"), " Step 1: Upload Data")),
        shiny::actionButton("launch_modal", list(shiny::icon("upload"), " Import data"), `aria-label` = "Import data", tabindex = 1),
        shiny::actionButton("about_app", list(shiny::icon("info-circle"), " About this app"), `aria-label` = "About this app", tabindex = 2),
        shiny::tags$hr(),
        shiny::tags$h4(list(shiny::icon("chart-bar"), " Step 2: Select Variables")),
        rintrojs::introBox(
          shiny::uiOutput("dependent_var_dropdown", `aria-label` = "Choose dependent variable", tabindex = 3),
          data.step = 3,
          data.intro = "Choose the dependent variable from this dropdown."
        ),
        rintrojs::introBox(
          shiny::textOutput("dependent_var_text"),
          data.step = 4,
          data.intro = "This is the dependent variable text."
        ),
        rintrojs::introBox(
          shiny::uiOutput("independent_var_dropdown", `aria-label` = "Choose independent variable", tabindex = 4),
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
        shiny::tags$h4(list(shiny::icon("flask"), " Step 3: Choose Test")),
        rintrojs::introBox(
          shiny::uiOutput("statistical_test_dropdown", `aria-label` = "Choose statistical test", tabindex = 5),
          data.step = 9,
          data.intro = "Choose the statistical test from this dropdown."
        ),
        shiny::tags$hr(),
        shiny::tags$h4(list(shiny::icon("table"), " Step 4: View Results"))
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Data Preview",
            shinycssloaders::withSpinner(DT::dataTableOutput("dataTable"))
          ),
          shiny::tabPanel("Variable Analysis",
            shiny::tags$div(
              shinycssloaders::withSpinner(shiny::plotOutput("dependent_var_histogram")),
              role = "img",
              `aria-label` = "Histogram of dependent variable"
            )
          ),
          shiny::tabPanel("Test Results",
            shiny::verbatimTextOutput("test_report")
          ),
          shiny::tabPanel("Interpretation",
            shiny::textOutput("interpretation_text") # Placeholder for interpretation guidance
          )
        )
      )
    )
  )
}
