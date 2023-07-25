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

    # Check if the selected dependent variable is numeric/float
    if (is.numeric(data()[, input$dependent_var])) {
      "The selected dependent variable is numeric/float."
    } else if (is.character(data()[, input$dependent_var])) {
      # If the selected dependent variable is character, return the number of unique values
      paste0("The selected dependent variable is character. Number of unique values: ", length(unique(data()[, input$dependent_var])))
    } else {
      "The selected dependent variable is not numeric or character."
    }
  })

  # Histogram of the dependent variable
  output$dependent_var_histogram <- renderPlot({
    req(data(), input$dependent_var)

    # Check if the selected dependent variable is numeric/float
    if (is.numeric(data()[, input$dependent_var])) {
      # Create the histogram plot
      hist(data()[, input$dependent_var], main = "Histogram", xlab = "Values")
    }
  })

  # Perform statistical test when dependent and independent variables are selected
  observeEvent(input$dependent_var, {
    req(input$dependent_var, input$independent_var)

    # Perform the statistical test using the selected variables
    # ...
  })
}
