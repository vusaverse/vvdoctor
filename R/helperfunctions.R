#' Display Data Table
#'
#' Helper function to display a data table in a Shiny app.
#'
#' @param data The data frame to display.
#'
#' @export
#'
display_data_table <- function(data) {
  DT::datatable(data)
}

#' Handle File Upload and Update Dropdowns
#'
#' Helper function for handling file upload and updating dropdowns in a Shiny app.
#'
#' @param input The input object from the Shiny app.
#' @param output The output object from the Shiny app.
#' @param session The session object from the Shiny app.
#'
#' @return A reactive value containing the uploaded data.
#'
#' @export
#'
handle_file_upload <- function(input, output, session) {
  # Reactive value to store the uploaded data
  data <- shiny::reactiveVal(NULL)

  # Observer for handling file upload
  shiny::observeEvent(input$file, {
    shiny::req(input$file)

    # Get the full path of the uploaded file
    full_path <- normalizePath(input$file$datapath)

    # Read the uploaded file
    data(utils::read.csv(full_path))

    # Populate the first dropdown with column names
    shiny::updateSelectInput(session, "independent_var", choices = c("", colnames(data())))
    shiny::updateSelectInput(session, "dependent_var", choices = c("", colnames(data())))
    # Set fileUploaded to TRUE to show the dependent and independent variable dropdowns
    shiny::updateNumericInput(session, "fileUploaded", value = 1)
  })

  # Return reactive data for further usage in the app
  return(data)
}



