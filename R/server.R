# Shiny App Server
#
# This file contains the server logic for the Shiny app.
#
# @importFrom DT datatable
# @export
app_server <- function(input, output, session) {
  # Reactive value to store the uploaded data
  data <- reactiveVal(NULL)

  # Observer for handling file upload
  observeEvent(input$file, {
    req(input$file)

    # Get the full path of the uploaded file
    full_path <- normalizePath(input$file$datapath)

    # Print the file path (optional, for debugging)
    print(full_path)

    # Read the uploaded file
    data(read.csv(full_path))

    # Populate the first dropdown with column names
    updateSelectInput(session, "independent_var", choices = colnames(data()))
    updateSelectInput(session, "dependent_var", choices = colnames(data()))
  })

  # Display an input field when the second dropdown has a "NULL" option
  output$mean_input <- renderUI({
    req(input$dependent_var)
    if (is.null(input$independent_var)) {
      shiny::numericInput("mean_val", "Enter Mean:", value = 0, step = 0.1)
    }
  })

  # Display the variable class info using a separate function
  output$variable_class_info <- renderText({
    req(input$independent_var)
    var_class_info <- get_variable_class_info(data()[[input$independent_var]])
    var_class_info
  })

  # Display the recommended statistical test based on the chosen variables
  output$recommended_test <- renderText({
    recommended_test <- choose_statistical_test(input$independent_var, input$dependent_var)
    paste("Recommended Statistical Test:", recommended_test)
  })

  # Plot histogram for continuous and normally distributed variable
  output$distPlot <- renderPlot({
    req(input$independent_var, data())

    # Get the variable class info
    var_class_info <- get_variable_class_info(data()[[input$independent_var]])

    if (is.numeric(data()[[input$independent_var]]) &&
        grepl("Continuous (Normally Distributed)", var_class_info[1])) {
      hist(data()[[input$independent_var]],
           main = "Histogram of Independent Variable",
           xlab = "Independent Variable",
           col = "darkgray")
    } else {
      NULL
    }
  })

  # Generate histogram for continuous dependent variable
  output$histPlot <- renderPlot({
    req(input$independent_var, input$dependent_var, data())

    if (input$dependent_var == "1" && input$independent_var == "continuous" &&
        is.numeric(data()[[input$independent_var]])) {
      hist(data()[[input$independent_var]],
           main = "Histogram of Continuous Dependent Variable",
           xlab = "Dependent Variable",
           col = "darkgray")
    } else {
      NULL
    }
  })

  # Display the uploaded data as a data table
  output$dataTable <- DT::renderDataTable({
    req(data())

    # Display the dataframe
    DT::datatable(data())
  })

  # # Display the column names of the uploaded dataframe
  # output$dataView <- renderPrint({
  #   req(data())
  #
  #   # Return the column names of the uploaded dataframe
  #   colnames(data())
  # })
}
