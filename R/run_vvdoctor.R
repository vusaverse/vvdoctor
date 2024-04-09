#' Run the Statistical Test App
#'
#' This function starts the Shiny app, which allows users to upload data and perform
#' statistical tests on the dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(vvdoctor)
#' run_statistical_test_app()
#' }
run_vvdoctor <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}


#' Diagnose Data with Statistical Tests
#'
#' This function launches a Shiny application that allows users to upload data
#' and perform various statistical tests on the dataset. Users can select the
#' dependent and independent variables, choose the appropriate statistical test,
#' and view the results.
#'
#' @return NULL The function does not return a value, but launches the Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' library(vvdoctor)
#' diagnose()
#' }
diagnose <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
