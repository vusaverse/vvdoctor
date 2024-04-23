#' App Server
#'
#' @param input The input object from the Shiny app.
#' @param output The output object from the Shiny app.
#' @param session The session object from the Shiny app.
#'
#' @export
app_server <- function(input, output, session) {
  shiny::observeEvent(input$launch_modal, {
    datamods::import_modal(
      id = "myid",
      from = c("env", "file", "copypaste", "googlesheets", "url"),
      title = "Import data to be used in application"
    )
  })

  imported <- datamods::import_server("myid", return_class = "data.frame")

  sdata <- shiny::reactive({
    if (is.null(imported$data())) {
      datasets::mtcars
    } else {
      shiny::req(imported$data())
      if (nrow(imported$data()) == 0) {
        datasets::mtcars
      } else {
        imported$data()
      }
    }
  })

  steps <- shiny::reactive({
    data.frame(
      element = c(NA, "#sep", "#header", "#dependent_var_dropdown", "#independent_var_dropdown", "#input_mean", "#identifier_dropdown", "#statistical_test_dropdown"),
      intro = c(
        "Welcome to the Statistical Test App. Let's start by uploading a file.",
        "Select the separator used in your file.",
        "Check this if your file has a header.",
        "Choose the dependent variable from this dropdown. The dependent variable is the outcome or response variable that is being studied and measured.",
        "Choose the independent variable from this dropdown. The independent is like the 'cause' or the 'input' that is being tested to see its effect on something else.",
        "Enter the mean here.",
        "Choose the identifier from this dropdown.",
        "Choose the statistical test from this dropdown. When should you use a paired or unpaired test? A paired test is used when the participants in the data are in both groups. For instance, if you want to research the effect of two different programs, and all the participants took both programs, you should use a paired test. If only part of the participants took one program and the other part of the participants took the other program, you should use an unpaired test. Other examples of unpaired tests would be differences between gender or A/B testing."
      )
    )
  })

  # Listen for the click event of the "About this app" action button
  shiny::observeEvent(input$about_app, {
    # Start the intro.js tour when the button is clicked
    rintrojs::introjs(session, options = list(steps = steps()))
  })

  # Display the uploaded data as a datatable
  output$dataTable <- DT::renderDataTable({
    shiny::req(sdata())
    DT::datatable(sdata())
  })

  # Dropdown for choosing the dependent variable
  output$dependent_var_dropdown <- shiny::renderUI({
    shiny::req(sdata())
    shinyWidgets::pickerInput("dependent_var", "Choose dependent variable", choices = colnames(sdata()))
  })

  # Dropdown for choosing the independent variable
  output$independent_var_dropdown <- shiny::renderUI({
    shiny::req(sdata())
    choices <- c("reference value", colnames(sdata()))
    shinyWidgets::pickerInput(
      "independent_var",
      "Choose independent variable or reference value",
      choices = choices
    )
  })

  output$input_mean <- shiny::renderUI({
    shiny::req(sdata())
    shiny::req(input$dependent_var)

    ## Retrieve the data from input
    data <- sdata()

    ## Calculate the mean of the data, if it's a valid value
    if (is.numeric(data[[input$dependent_var]])) {
      reference_value <- mean(data[[input$dependent_var]], na.rm = TRUE)
    } else {
      reference_value <- NULL
    }

    ## Only show input field if reference value is selected
    if (input$independent_var == "reference value") {
      shiny::numericInput("input_mean", "Set reference value", value = reference_value)
    }
  })

  # Dropdown for choosing the dependent variable
  output$identifier_dropdown <- shiny::renderUI({
    shiny::req(sdata())
    shinyWidgets::pickerInput("identifier_var", "Choose identifier variable", choices = colnames(sdata()))
  })

  # Text below the dropdowns
  output$dependent_var_text <- shiny::renderText({
    shiny::req(input$dependent_var)
    determine_dependent_variable(sdata()[, input$dependent_var])
  })

  # Additional text for independent variable
  output$independent_var_text <- shiny::renderText({
    shiny::req(input$independent_var)

    if (input$independent_var %in% colnames(sdata())) {
      determine_independent_variable(sdata()[, input$independent_var])
    }
  })

  # New dropdown for selecting statistical test
  output$statistical_test_dropdown <- shiny::renderUI({
    shiny::req(input$dependent_var, input$independent_var)

    if (input$independent_var == "reference value") {
      independent_var <- "reference value"
    } else {
      independent_var <- sdata()[, input$independent_var]
    }
    test <- choose_statistical_test(sdata()[, input$dependent_var], independent_var)
    test_options <- c(test)
    shinyWidgets::pickerInput("statistical_test", "Choose statistical test", choices = test_options)
  })

  # Histogram of the dependent variable
  output$dependent_var_histogram <- shiny::renderPlot({
    shiny::req(sdata(), input$dependent_var)
    create_dependent_variable_histogram(sdata()[, input$dependent_var])
  })

  ## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ## X. Statistical tests ####
  ## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  output$test_report <- shiny::renderPrint({
    result <- perform_statistical_test(sdata(), input)
    if (!is.null(result)) result
  })
}
