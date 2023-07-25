# Shiny App Server
#' @export
app_server <- function(input, output, session) {
  data <- handle_file_upload(input, output, session)

  # Display the uploaded data as a datatable
  output$dataTable <- DT::renderDataTable({
    req(data())
    DT::datatable(data())
  })
}


#' #' Title
#' #'
#' #'
#' #'
#' app_server <- function(input, output, session) {
#'   data <- handle_file_upload(input, output, session)
#'
#'   # Reactive value to track whether a file has been uploaded
#'   observe({
#'     req(data())
#'     # Set fileUploaded to TRUE to show the dependent and independent variable dropdowns
#'     updateNumericInput(session, "fileUploaded", value = 1)
#'   })
#'
#'   # Display an input field when the second dropdown has a "NULL" option
#'   output$mean_input <- renderUI({
#'     req(input$dependent_var)
#'     if (is.null(input$independent_var) || input$independent_var == "") {
#'       shiny::numericInput("mean_val", "Enter Mean:", value = 0, step = 0.1)
#'     }
#'   })
#'
#'   # Display the variable class info using a separate function
#'   output$variable_class_info <- renderText({
#'     req(input$independent_var)
#'     var_class_info <- get_variable_class_info(data()[[input$independent_var]])
#'     var_class_info
#'   })
#'
#'   # Display the recommended statistical test based on the chosen variables
#'   output$recommended_test <- renderText({
#'     recommended_test <- choose_statistical_test(input$independent_var, input$dependent_var)
#'     paste("Recommended Statistical Test:", recommended_test)
#'   })
#'
#'   # Plot histogram for continuous and normally distributed variable
#'   output$distPlot <- renderPlot({
#'     req(input$independent_var, data())
#'
#'     # Get the variable class info
#'     var_class_info <- get_variable_class_info(data()[[input$independent_var]])
#'
#'     if (is.numeric(data()[[input$independent_var]]) &&
#'         grepl("Continuous (Normally Distributed)", var_class_info[1])) {
#'       hist(data()[[input$independent_var]],
#'            main = "Histogram of Independent Variable",
#'            xlab = "Independent Variable",
#'            col = "darkgray")
#'     } else {
#'       NULL
#'     }
#'   })
#'
#'   # Generate histogram for continuous dependent variable
#'   output$histPlot <- renderPlot({
#'     req(input$independent_var, input$dependent_var, data())
#'
#'     if (input$dependent_var == "1" && input$independent_var == "continuous" &&
#'         is.numeric(data()[[input$independent_var]])) {
#'       hist(data()[[input$independent_var]],
#'            main = "Histogram of Continuous Dependent Variable",
#'            xlab = "Dependent Variable",
#'            col = "darkgray")
#'     } else {
#'       NULL
#'     }
#'   })
#'
#'   # Display the uploaded data as a data table
#'   output$dataTable <- renderDataTable({
#'     req(data())
#'
#'     # Display the dataframe using the helper function
#'     display_data_table(data())
#'   })
#'
#'   # Display information about the dependent variable
#'   output$dependent_var_info <- renderText({
#'     req(input$dependent_var, data())
#'
#'     dependent_var <- data()[[input$dependent_var]]
#'
#'     if (is.numeric(dependent_var)) {
#'       # Check for normality of continuous dependent variable
#'       var_class_info <- get_variable_class_info(dependent_var)
#'       return(var_class_info)
#'     } else {
#'       # Check if the categorical variable is binary or nominal
#'       unique_values <- unique(dependent_var)
#'       if (length(unique_values) == 2) {
#'         return("Dependent Variable: Categorical (Binary)")
#'       } else {
#'         return("Dependent Variable: Categorical (Nominal)")
#'       }
#'     }
#'   })
#' }
#'
#'
#'
