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

  # Define a wrapper function that ignores sep and header arguments
  ignore_args <- function(func) {
    function(path, sep = NULL, header = NULL) {
      func(path)
    }
  }

  # Map file extensions to reading functions
  read_funcs <- list(
    RData = readRDS,
    asc = ignore_args(utils::read.table),
    csv = function(path, sep, header) utils::read.csv(path, sep = sep, header = header),
    feather = ignore_args(feather::read_feather),
    fst = ignore_args(fst::read_fst),
    parquet = ignore_args(arrow::read_parquet),
    rda = readRDS,
    rds = readRDS,
    sav = ignore_args(haven::read_sav),
    tsv = function(path, sep, header) utils::read.delim(path, sep = sep, header = header),
    txt = function(path, sep, header) utils::read.delim(path, sep = sep, header = header),
    xlsx = ignore_args(readxl::read_excel)
  )

  # Observer for handling file upload
  shiny::observeEvent(input$file, {
    shiny::req(input$file)

    # Get the full path of the uploaded file
    full_path <- normalizePath(input$file$datapath)

    # Get the file extension
    ext <- tools::file_ext(full_path)

    # Get the separator, delimiter, and header from the user input
    sep <- input$sep
    header <- input$header

    # Read the uploaded file based on its extension
    tryCatch({
      data(read_funcs[[ext]](full_path, sep, header))
    }, error = function(e) {
      # Log the error
      message("An error occurred: ", e$message)

      # Reset the data reactive value
      data(NULL)
    })

    # Populate the first dropdown with column names
    shiny::updateSelectInput(session, "independent_var", choices = c("", colnames(data())))
    shiny::updateSelectInput(session, "dependent_var", choices = c("", colnames(data())))
    # Set fileUploaded to TRUE to show the dependent and independent variable dropdowns
    shiny::updateNumericInput(session, "fileUploaded", value = 1)
  })

  # Return reactive data for further usage in the app
  return(data)
}
