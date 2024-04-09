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
  # data <- shiny::reactiveVal(NULL)
  data <- shiny::reactiveVal(datasets::mtcars)

  # Function to dynamically generate dataset names
  # This is a placeholder. Replace with your actual logic.
  get_dataset_names <- function() {
    # Example: Return a vector of dataset names
    # In a real application, you might fetch this from a database or another source
    dataset_names <- data()$results[,3]
    # return(paste(dataset_names,collapse=" "))
    # return(c(dataset_names))
    return(c("mtcars", "iris", "airquality"))
  }

  # Add a dropdown for loading R datasets
  output$dataset_dropdown <- shiny::renderUI({
    # Use the function to get dataset names
    dataset_names <- get_dataset_names()
    # Create a named vector with dataset names as both names and values
    dataset_choices <- stats::setNames(dataset_names, dataset_names)
    # Add "None" as the first choice with an empty string value
    dataset_choices <- c("None" = "", dataset_choices)
    shiny::selectInput("dataset", "Load R Dataset", choices = dataset_choices)
  })


  shiny::observeEvent(input$dataset, {
    dataset_name <- input$dataset
    if (nchar(dataset_name) > 0) {
      # Load the selected dataset
      data_set <- get(dataset_name, envir = as.environment("package:datasets"))
      # Update the data reactive value with the loaded dataset
      data(data_set)

      # Populate the dropdowns with column names
      shiny::updateSelectInput(session, "independent_var", choices = c("", colnames(data())))
      shiny::updateSelectInput(session, "dependent_var", choices = c("", colnames(data())))

      # Set fileUploaded to TRUE to show the dependent and independent variable dropdowns
      shiny::updateNumericInput(session, "fileUploaded", value = 1)
    }
  })
  # Define a wrapper function that ignores sep and header arguments
  ignore_args <- function(func) {
    function(path, sep = NULL, header = NULL) {
      func(path)
    }
  }

  # Map file extensions to reading functions
  read_funcs <- list(
    RData = ignore_args(readRDS),
    asc = ignore_args(utils::read.table),
    csv = function(path, sep, header) utils::read.csv(path, sep = sep, header = header),
    feather = ignore_args(feather::read_feather),
    fst = ignore_args(fst::read_fst),
    parquet = ignore_args(arrow::read_parquet),
    rda = ignore_args(readRDS),
    rds = ignore_args(readRDS),
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
    tryCatch(
      {
        data(read_funcs[[ext]](full_path, sep, header))
      },
      error = function(e) {
        # Log the error
        message("An error occurred: ", e$message)

        # Reset the data reactive value
        data(NULL)
      }
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



# Determine the type of dependent variable
determine_dependent_variable <- function(dependent_var) {
  # Check if the selected dependent variable is numeric/float
  if (is.numeric(dependent_var)) {
    # Perform Shapiro-Wilk test on the dependent variable
    shapiro_test <- stats::shapiro.test(dependent_var)

    # Check if the p-value is less than 0.05
    if (shapiro_test$p.value < 0.05) {
      "Assumption of Normality violated by the dependent variable."
    } else {
      "Assumption of Normality met by the dependent variable."
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
    graphics::hist(dependent_var, main = "Histogram of Dependent Variable", xlab = "Values")
  }
}
