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

    # Get the file extension
    ext <- tools::file_ext(full_path)

    # Read the uploaded file based on its extension
    switch(ext,
           RData = data(readRDS(full_path)),
           asc = data(utils::read.table(full_path, header = TRUE)),
           csv = data(utils::read.csv(full_path)),
           feather = data(feather::read_feather(full_path)),
           fst = data(fst::read_fst(full_path)),
           parquet = data(arrow::read_parquet(full_path)),
           rda = data(readRDS(full_path)),
           rds = data(readRDS(full_path)),
           sav = data(haven::read_sav(full_path)),
           tsv = data(utils::read.delim(full_path, sep = "\t")),
           txt = data(utils::read.delim(full_path, sep = "\t")),
           xlsx = data(readxl::read_excel(full_path)),
           stop("Invalid file type. Please upload a .RData, .asc, .csv, .feather, .fst, .parquet, .rda, .rds, .sav, .tsv, .txt or .xlsx file.")
    )

    # Populate the first dropdown with column names
    shiny::updateSelectInput(session, "independent_var", choices = c("", colnames(data())))
    shiny::updateSelectInput(session, "dependent_var", choices = c("", colnames(data())))
    # Set fileUploaded to TRUE to show the dependent and independent variable dropdowns
    shiny::updateNumericInput(session, "fileUploaded", value = 1)
  })

  # Return reactive data for further usage in the app
  return(data)
}
