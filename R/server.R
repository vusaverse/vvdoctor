
#' Title
#'
#' @export
#'
app_server <- function(input, output, session) {
  data <- handle_file_upload(input, output, session)

  # Display the uploaded data as a datatable
  output$dataTable <- DT::renderDataTable({
    req(data())
    DT::datatable(data())
  })

  # Dropdown for choosing the dependent variable
  output$dependent_var_dropdown <- renderUI({
    req(data())
    shiny::selectInput("dependent_var", "Choose dependent variable", choices = colnames(data()))
  })

  # Dropdown for choosing the independent variable
  output$independent_var_dropdown <- renderUI({
    req(data())
    shiny::selectInput("independent_var", "Choose independent variable", choices = c("None", colnames(data())))
  })

  # Text below the dropdowns
  output$dependent_var_text <- renderText({
    req(input$dependent_var)
    determine_dependent_variable(data()[, input$dependent_var])
  })

  # Additional text for independent variable
  output$independent_var_text <- renderText({
    req(input$independent_var)
    determine_independent_variable(data()[, input$independent_var])
  })

  # New dropdown for selecting statistical test
  output$statistical_test_dropdown <- renderUI({
    req(input$dependent_var, input$independent_var)
    test <- choose_statistical_test(data()[, input$dependent_var], data()[, input$independent_var], paired = input$paired_unpaired == "Paired")
    test_options <- c("None", test)
    shiny::selectInput("statistical_test", "Choose statistical test", choices = test_options)
  })

  # Histogram of the dependent variable
  output$dependent_var_histogram <- renderPlot({
    req(data(), input$dependent_var)
    create_dependent_variable_histogram(data()[, input$dependent_var])
  })

  # Perform statistical test when dependent and independent variables are selected
  observeEvent(input$dependent_var, {
    req(input$dependent_var, input$independent_var)
    perform_statistical_test(data(), input$dependent_var, input$independent_var)
  })

  # Determine the type of dependent variable
  determine_dependent_variable <- function(dependent_var) {
    # Check if the selected dependent variable is numeric/float
    if (is.numeric(dependent_var)) {
      # Perform Shapiro-Wilk test on the dependent variable
      shapiro_test <- shapiro.test(dependent_var)

      # Check if the p-value is less than 0.05
      if (shapiro_test$p.value < 0.05) {
        "The selected dependent variable is not normally distributed."
      } else {
        "The selected dependent variable is normally distributed."
      }
    } else if (is.character(dependent_var)) {
      # Check if the selected dependent variable has 2 unique values (binary) or more than 2 (nominal/ordinal)
      unique_values <- length(unique(dependent_var))
      if (unique_values == 2) {
        "The selected dependent variable is binary."
      } else if (unique_values > 2) {
        "The selected dependent variable is nominal/ordinal."
      } else {
        "The selected dependent variable has invalid data."
      }
    } else {
      "The selected dependent variable is not numeric or character."
    }
  }

  # Determine the type of independent variable
  determine_independent_variable <- function(independent_var) {
    # Check if the selected independent variable is numeric/float
    if (is.numeric(independent_var)) {
      "The selected independent variable is numeric."
    } else if (is.character(independent_var)) {
      # Check if the selected independent variable has 2 unique values or more than 2
      unique_values <- length(unique(independent_var))
      if (unique_values == 2) {
        "The selected independent variable has 2 unique values."
      } else if (unique_values > 2) {
        "The selected independent variable has more than 2 unique values."
      } else {
        "The selected independent variable has invalid data."
      }
    } else {
      "The selected independent variable is not numeric or character."
    }
  }

  # Create the histogram plot for the dependent variable
  create_dependent_variable_histogram <- function(dependent_var) {
    if (is.numeric(dependent_var)) {
      hist(dependent_var, main = "Histogram of Dependent Variable", xlab = "Values")
    }
  }

  # Perform the statistical test using the selected variables
  perform_statistical_test <- function(data, dependent_var, independent_var) {
    # ...
  }
}
