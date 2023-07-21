# Helper function to display the data table
#' Title
#'
#'
#' @export
#'
display_data_table <- function(data) {
  DT::datatable(data)
}

# Helper function for handling file upload and updating dropdowns
#' Title
#'
#'
#' @export
#'
handle_file_upload <- function(input, output, session) {
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
    updateSelectInput(session, "independent_var", choices = c("", colnames(data())))
    updateSelectInput(session, "dependent_var", choices = c("", colnames(data())))
    # Set fileUploaded to TRUE to show the dependent and independent variable dropdowns
    updateNumericInput(session, "fileUploaded", value = 1)
  })

  # Return reactive data for further usage in the app
  return(data)
}



