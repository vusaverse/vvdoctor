#' Shiny App Server
#'
#' This file contains the server logic for the Shiny app.
#'
#' @importFrom DT datatable
#' @export
app_server <- function(input, output, session) {
  data <- reactive({
    req(input$file)

    # Get the full path of the uploaded file
    full_path <- normalizePath(input$file$datapath)

    # Print the file path
    print(full_path)

    # Read the uploaded file
    read.csv(full_path)
  })

  # Get the column names of the dataframe and ensure they are unique
  column_names <- reactive({
    req(data())
    colnames(data())
  })

  # Remove the selected column from the second dropdown
  observe({
    req(input$col_select_1)
    updateSelectInput(session, "col_select_2", choices = setdiff(column_names(), input$col_select_1))
  })

  # Display an input field when the second dropdown has a "NULL" option
  output$mean_input <- renderUI({
    if (is.null(input$col_select_2)) {
      shiny::numericInput("mean_val", "Enter Mean:", value = 0, step = 0.1)
    }
  })

  output$dataTable <- DT::renderDataTable({
    req(data())

    # Display the dataframe
    DT::datatable(data())
  })

  output$dataView <- renderPrint({
    req(data())

    # Return the column names of the uploaded dataframe
    colnames(data())
  })
}
